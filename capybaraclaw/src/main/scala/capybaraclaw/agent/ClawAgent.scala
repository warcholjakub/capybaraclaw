package capybaraclaw.agent

import tacit.core.{
  ApiMode,
  Context as TacitContext,
  Config as TacitConfig,
  LoadedPlugin,
  PluginLoader,
}
import tacit.executor.ReplSession
import tacit.agents.llm.endpoint.*
import tacit.agents.llm.agentic.{Agent, AgentRun, AgentState, AgentError}
import gears.async.Async
import tacit.agents.llm.utils.IsToolArg
import tacit.agents.utils.Result
import io.circe.Json
import io.circe.syntax.*
import tacit.library.Interface as TacitLibraryInterface
import scala.util.Try

case class EvalScalaArgs(code: String) derives IsToolArg
case class ShowInterfaceArgs() derives IsToolArg

/** Agent class for Claw. */
class ClawAgent(
    val workDir: String,
    initialMessages: List[Message] = Nil,
    endpointOverride: Option[Endpoint] = None
):
  val agentConfig: AgentConfig = AgentConfig.load(workDir)

  // The safe-libs REPL preamble reads `safemode.workdir` to scope its
  // `given FileSystem[FullAccess]` to this directory. Setting the JVM-wide
  // property here, before TacitContext starts the REPL, means the preamble
  // sees the right path when it runs. Last agent in this JVM wins; that's
  // fine for the current single-workdir gateway.
  System.setProperty("safemode.workdir", workDir)
  agentConfig.privateLlm match
    case Some(privateLlm) =>
      System.setProperty("safemode.private.provider", privateLlm.provider)
      System.setProperty("safemode.private.baseUrl", privateLlm.baseUrl)
      System.setProperty("safemode.private.model", privateLlm.model)
    case None =>
      System.clearProperty("safemode.private.provider")
      System.clearProperty("safemode.private.baseUrl")
      System.clearProperty("safemode.private.model")

  private val pluginsDir: String =
    java.io.File(workDir, "plugins").getCanonicalPath

  private val pluginScanDirs: List[String] =
    if java.io.File(pluginsDir).isDirectory then List(pluginsDir) else Nil

  private val loadedPlugins: List[LoadedPlugin] =
    PluginLoader.loadAll(jars = Nil, scanDirs = pluginScanDirs) match
      case Left(err) =>
        System.err.println(s"Error: $err")
        sys.exit(1)
      case Right(plugins) => plugins

  private val tacitContext: TacitContext = TacitContext(
    TacitConfig(
      libraryJarPath = resolveTacitLibraryJarPath(),
      libraryConfig = Json.obj(
        "classifiedPaths" -> agentConfig.classifiedPaths
          .map(p => java.io.File(workDir, p).getCanonicalPath)
          .asJson
      ),
      pluginScanDirs = pluginScanDirs,
    ),
    recorder = None,
    plugins = loadedPlugins,
  )
  private val repl: ReplSession = ReplSession.create(using tacitContext)

  private given Endpoint = endpointOverride.getOrElse(agentConfig.provider match
    case "anthropic"  => AnthropicEndpoint.createFromEnv()
    case "openai"     => OpenAIEndpoint.createFromEnv()
    case "openrouter" => OpenRouterEndpoint.createFromEnv()
    case "ollama"     => OllamaEndpoint.createFromEnv()
    case other        => throw RuntimeException(s"Unknown provider: $other"))

  private val agent: Agent =
    val a = new Agent:
      type State = AgentState
      def getInitState = new AgentState:
        val llmConfig = agentConfig.toLLMConfig

    a.handle[ShowInterfaceArgs](
      "show_interface",
      "Returns the exact capability-scoped API available in this REPL. Call " +
        "this BEFORE your first evaluate_scala — guessing method names wastes " +
        "turns. The REPL preamble pre-loads available imports, so refer to the " +
        "documented symbols directly."
    ): (_, _) =>
      ClawAgent.composedInterfaceReference(agentConfig, loadedPlugins)

    a.handle[EvalScalaArgs](
      "evaluate_scala",
      "Evaluate a Scala expression in a persistent REPL session. The REPL has " +
        "Scala 3 capture-checking enabled, so confidential-data exfiltration " +
        "attempts (capturing IOCapability / Network / FileSystem in a pure " +
        "arrow) are rejected at compile time. State persists across calls."
    ): (args, _) =>
      val result = repl.execute(args.code)
      if result.success then
        if result.output.nonEmpty then result.output
        else "(executed successfully, no output)"
      else
        val msg = StringBuilder("Execution failed.\n")
        if result.output.nonEmpty then
          msg.append(s"Output:\n${result.output}\n")
        result.error.foreach(e => msg.append(s"Error:\n$e\n"))
        msg.toString

    // Seed with any persisted prior transcript so rehydrated conversations continue
    // where they left off.
    a.state.messages = initialMessages

    a

  def ask(
      message: String,
      onToolCall: Option[(String, String, String) => Unit] = None
  ): Result[ChatResponse, AgentError] =
    agent.ask(message, onToolCall)

  def streamAsk(message: String)(using Async.Spawn): AgentRun =
    agent.streamAsk(message)

  def printStartupInfo(): Unit =
    val clawJsonExists = java.io.File(workDir, "claw.json").exists()
    val clawMdExists = java.io.File(workDir, "CLAW.md").exists()
    println("Capybara Claw")
    println(s"  workdir  : $workDir")
    println(s"  provider : ${agentConfig.provider}")
    println(s"  model    : ${agentConfig.model}")
    println(s"  thinking : ${agentConfig.thinking.getOrElse("off")}")
    println(s"  claw.json: ${if clawJsonExists then "found" else "defaults"}")
    println(s"  CLAW.md  : ${if clawMdExists then "found" else "not found"}")
    if agentConfig.classifiedPaths.nonEmpty then
      println(s"  classify : ${agentConfig.classifiedPaths.mkString(", ")}")
    if loadedPlugins.nonEmpty then
      val rows = loadedPlugins.map: p =>
        val m = p.manifest
        s"${m.name} ${m.version} (${ClawAgent.renderApiMode(m.apiMode)})"
      println(s"  plugins  : ${rows.mkString(", ")}")
    else if java.io.File(pluginsDir).isDirectory then
      println(s"  plugins  : (none in $pluginsDir)")
    println()

  private def resolveTacitLibraryJarPath(): String =
    val fromProperty =
      Option(System.getProperty("tacit.library.jar")).filter(_.nonEmpty)
    val fromCodeSource =
      Try:
        val url = classOf[
          TacitLibraryInterface
        ].getProtectionDomain.getCodeSource.getLocation
        java.io.File(url.toURI).getAbsolutePath
      .toOption
        .filter(_.nonEmpty)

    fromProperty
      .orElse(fromCodeSource)
      .getOrElse:
        throw RuntimeException(
          "Unable to resolve tacit-library path. Set -Dtacit.library.jar or ensure tacit-library is on classpath."
        )

object ClawAgent:
  /** Compose the `show_interface` output from the API surface actually loaded
    * into the REPL. When any plugin uses `replace-core`, the core tacit
    * `Interface.scala` is intentionally hidden from the model — its symbols
    * are not imported and surfacing them would invite hallucinated calls.
    */
  def composedInterfaceReference(
      agentConfig: AgentConfig,
      plugins: List[LoadedPlugin],
  ): String =
    val sb = StringBuilder()
    sb.append(
      """|IMPORTANT: You must only use the provided interface below to interact
         |with the system. Do not use Java/Scala stdlib APIs (java.io, java.nio,
         |scala.io, sys.process, java.net, etc.) directly — they are blocked by
         |the REPL's code validator. All side effects must go through the
         |capability-scoped API so they are properly sandboxed.
         |
         |The interface is pre-loaded and available in all evaluate_scala calls.
         |
         |""".stripMargin
    )

    if plugins.nonEmpty then
      val includeCore = plugins.forall(_.manifest.apiMode == ApiMode.ExtendCore)
      sb.append("# Loaded plugins\n\n")
      plugins.foreach: p =>
        val m = p.manifest
        val domainPart = m.domain.fold("")(d => s" — $d")
        sb.append(s"## ${m.name} ${m.version}$domainPart\n")
        sb.append(s"Mode: ${renderApiMode(m.apiMode)}\n")
        m.description.foreach(d => sb.append(s"$d\n"))
        sb.append("\n")
      sb.append("# Available API\n\n")
      if includeCore then
        sb.append(coreInterfaceReference).append("\n\n---\n\n")
      sb.append(
        plugins.map(p => s"## ${p.manifest.name}\n\n${p.apiDocs}")
          .mkString("\n\n---\n\n")
      )
    else sb.append(coreInterfaceReference)

    // Closing nudge. Small models tend to "reflect on the architecture" after
    // a long doc dump instead of returning to the user's task. The directive
    // below pushes them to immediately call evaluate_scala.
    sb.append(
      """|
         |
         |---
         |
         |**Now return to the user's question.** Your next response MUST be a
         |`evaluate_scala` tool call containing the Scala code that answers it.
         |If that code successfully writes the requested confidential answer,
         |stop calling tools; repeated writes append duplicate output.
         |Do NOT write commentary on the API design, do NOT summarize the
         |interface, do NOT emit free-form code in chat — none of those run.
         |""".stripMargin
    )
    sb.toString

  def renderApiMode(mode: ApiMode): String = mode match
    case ApiMode.ExtendCore  => "extend-core"
    case ApiMode.ReplaceCore => "replace-core"

  private def coreInterfaceReference: String =
    val stream = classOf[ClawAgent].getClassLoader
      .getResourceAsStream("Interface.scala")
    if stream == null then
      "(Interface.scala not found on classpath — this is a build issue)"
    else
      try
        val content = scala.io.Source.fromInputStream(stream).mkString
        "```scala\n" + content + "\n```"
      finally stream.close()
