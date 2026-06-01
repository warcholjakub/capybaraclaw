package capybaraclaw.agent

import tacit.agents.llm.endpoint.{EffortLevel, LLMConfig, ThinkingMode}

/** Private model used by safe-libs for confidential post-processing. This is
  * intentionally separate from the public agent model in `claw.json`.
  */
case class PrivateLlmConfig(
    provider: String = "ollama",
    baseUrl: String = "http://localhost:11434",
    model: String = "qwen3.5:latest"
)

/** Configuration for a Claw agent instance.
  */
case class AgentConfig(
    workDir: String,
    provider: String = "openrouter",
    model: String = "minimax/minimax-m2.7",
    maxTokens: Int = 16000,
    thinking: Option[ThinkingMode] = None,
    classifiedPaths: List[String] = Nil,
    /** True when `${workDir}/plugins/` exists and contains at least one `*.jar`.
      * Derived at load time; informs only the system-prompt hint about which
      * classified-output function the agent should use. The authoritative
      * plugin list is read by TACIT via `pluginScanDirs`.
      */
    pluginsConfigured: Boolean = false,
    privateLlm: Option[PrivateLlmConfig] = None
):
  def toLLMConfig: LLMConfig =
    LLMConfig(
      model = model,
      systemPrompt = Some(AgentConfig.buildSystemPrompt(this)),
      maxTokens = Some(maxTokens),
      thinking = thinking
    )

object AgentConfig:
  /** Load `${workDir}/claw.json` if present; otherwise use defaults. The `thinking`
    * mode is derived from the provider unless explicitly set in the JSON.
    */
  def load(workDir: String): AgentConfig =
    val file = java.io.File(workDir, "claw.json")
    val obj =
      if file.exists() then
        ujson.read(scala.io.Source.fromFile(file).mkString).obj
      else ujson.Obj().value
    val provider = obj.get("provider").map(_.str).getOrElse("openrouter")
    AgentConfig(
      workDir = workDir,
      provider = provider,
      model = obj.get("model").map(_.str).getOrElse("minimax/minimax-m2.7"),
      maxTokens = obj.get("max_tokens").map(_.num.toInt).getOrElse(16000),
      thinking = deriveThinking(provider),
      classifiedPaths =
        obj.get("classified_paths").map(_.arr.map(_.str).toList).getOrElse(Nil),
      pluginsConfigured = detectPluginsConfigured(workDir),
      privateLlm = loadPrivateLlm(workDir, obj)
    )

  /** True when `${workDir}/plugins/` is a directory containing at least one
    * `*.jar`. The actual plugin loading happens inside TACIT — capybaraclaw
    * only uses this flag to phrase the system prompt's classified-output hint.
    */
  private def detectPluginsConfigured(workDir: String): Boolean =
    val dir = java.io.File(workDir, "plugins")
    dir.isDirectory && Option(dir.list((_, n) => n.endsWith(".jar")))
      .exists(_.nonEmpty)

  private def loadPrivateLlm(
      workDir: String,
      obj: scala.collection.Map[String, ujson.Value]
  ): Option[PrivateLlmConfig] =
    val configName = obj
      .get("private_llm_config")
      .map(_.str)
      .getOrElse("claw.private.json")
    val file = java.io.File(workDir, configName)
    if !file.exists() then None
    else
      val privateObj = ujson.read(scala.io.Source.fromFile(file).mkString).obj
      Some(
        PrivateLlmConfig(
          provider = privateObj.get("provider").map(_.str).getOrElse("ollama"),
          baseUrl = privateObj
            .get("base_url")
            .orElse(privateObj.get("url"))
            .map(_.str)
            .getOrElse("http://localhost:11434"),
          model = privateObj.get("model").map(_.str).getOrElse("qwen3.5:latest")
        )
      )

  private def deriveThinking(provider: String): Option[ThinkingMode] =
    provider match
      case "anthropic"             => Some(ThinkingMode.Budget(2048))
      case "openai" | "openrouter" =>
        Some(ThinkingMode.Effort(EffortLevel.Medium))
      // Ollama's /v1/responses accepts `reasoning` but doesn't actually run
      // reasoning tokens — it just disturbs tool-call emission on non-thinking
      // models (qwen2.5 etc. start replying in prose instead of tool_calls).
      // Leave None and let users opt in via claw.json if they're on a thinking
      // model.
      case "ollama" => None
      case _        => None

  private def loadClawMd(workDir: String): Option[String] =
    val file = java.io.File(workDir, "CLAW.md")
    if file.exists() then Some(scala.io.Source.fromFile(file).mkString)
    else None

  private def buildSystemPrompt(config: AgentConfig): String =
    val clawMd = loadClawMd(config.workDir)

    val sb = StringBuilder()

    sb.append(s"""<role>
You execute Scala code in a sandboxed REPL. You MUST interact via tools. NEVER write code or answers in plain chat — write code in the `evaluate_scala` tool's `code` argument, and read its `output` for the result. Free-form code blocks in your reply text are ignored — they don't run.

Available tools:
- `show_interface` — returns the exact API surface loaded for this workdir. Call this ONCE at the start of any new task before writing code. Do NOT guess method names.
- `evaluate_scala` — runs a Scala snippet in the persistent REPL session. State carries across calls. Capture checking is on: lambdas under `ConfidentialColumn.map/filter` or `Classified.map` are pure arrows `T -> U` and reject `IOCapability`, `Network`, writable `FileSystem`.

If an `evaluate_scala` call successfully writes the requested confidential answer, STOP. Do not call tools again to verify the write; the output sink is append-only, so repeated writes duplicate the answer.

Workdir: ${config.workDir}
</role>""")

    if config.classifiedPaths.nonEmpty then
      val outputHint =
        if config.pluginsConfigured then
          "Use the plugin's documented classified output function, e.g. `writePrivateAnswer`, exactly as shown by `show_interface`."
        else "Use `writeClassified` exactly as shown by `show_interface`."
      sb.append(s"""

<classified_paths>
The following paths are classified. Reading them yields `Classified[T]`; you cannot unwrap classified values or print them directly. $outputHint
${config.classifiedPaths.map(p => s"- $p").mkString("\n")}
</classified_paths>""")

    clawMd.foreach: md =>
      sb.append(s"""

<project_instructions>
$md
</project_instructions>""")

    sb.toString
