import tacit.executor.ScalaExecutor
import tacit.core.{Context, Config}
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

class PermissionOracleSuite extends munit.FunSuite:
  private def tempDir(prefix: String): Path =
    Files.createTempDirectory(prefix).toRealPath()

  private def contextFor(
    allowed: Path,
    oracle: Option[String => String],
    commands: Set[String] = Set.empty,
    hosts: Set[String] = Set.empty,
  ): Context =
    def strings(xs: Set[String]) = io.circe.Json.arr(xs.toList.map(io.circe.Json.fromString)*)
    Context(
      Config(libraryConfig = io.circe.Json.obj(
        "allowedRoots" -> strings(Set(allowed.toString)),
        "commandPermissions" -> strings(commands),
        "networkPermissions" -> strings(hosts),
      )),
      None,
      permissionOracle = oracle,
    )

  private def itemsOf(request: String): (String, List[String]) =
    val json = io.circe.parser.parse(request).toOption.get.hcursor
    (json.get[String]("kind").toOption.get, json.get[List[String]]("items").toOption.get)

  private def existsCode(dir: Path): String =
    s"""requestFileSystem("$dir") { access("$dir").exists }"""

  test("a root outside allowedRoots is granted when the oracle allows it"):
    val allowed = tempDir("allowed")
    val outside = tempDir("outside")
    val requests = ConcurrentLinkedQueue[String]()
    given Context = contextFor(allowed, Some: request =>
      requests.add(request)
      """{"allow":true}""")
    val result = ScalaExecutor.execute(existsCode(outside))
    assert(result.success, s"execution failed: ${result.error.getOrElse(result.output)}")
    assert(result.output.contains("true"), result.output)
    val List(request) = requests.asScala.toList: @unchecked
    val json = io.circe.parser.parse(request).toOption.get.hcursor
    assertEquals(json.get[String]("kind"), Right("filesystem"))
    assertEquals(json.get[String]("resolved"), Right(outside.toString))

  test("a denial carries the oracle's message"):
    val allowed = tempDir("allowed")
    val outside = tempDir("outside")
    given Context = contextFor(allowed, Some(_ =>
      """{"allow":false,"message":"needs the user's approval (request #3)"}"""))
    val result = ScalaExecutor.execute(existsCode(outside))
    assert(result.output.contains("SecurityException"), result.output)
    assert(result.output.contains("needs the user's approval (request #3)"), result.output)

  test("without an oracle, a root outside allowedRoots is denied as before"):
    val allowed = tempDir("allowed")
    val outside = tempDir("outside")
    given Context = contextFor(allowed, None)
    val result = ScalaExecutor.execute(existsCode(outside))
    assert(result.output.contains("not within any allowed root"), result.output)

  test("an answer the library cannot parse is a denial"):
    val allowed = tempDir("allowed")
    val outside = tempDir("outside")
    given Context = contextFor(allowed, Some(_ => "yes please"))
    val result = ScalaExecutor.execute(existsCode(outside))
    assert(result.output.contains("not within any allowed root"), result.output)

  test("an oracle that fails is a denial"):
    val allowed = tempDir("allowed")
    val outside = tempDir("outside")
    given Context = contextFor(allowed, Some(_ => throw IllegalStateException("host is down")))
    val result = ScalaExecutor.execute(existsCode(outside))
    assert(result.output.contains("not within any allowed root"), result.output)
    assert(!result.output.contains("host is down"), result.output)

  test("roots inside allowedRoots do not consult the oracle"):
    val allowed = tempDir("allowed")
    val requests = ConcurrentLinkedQueue[String]()
    given Context = contextFor(allowed, Some: request =>
      requests.add(request)
      """{"allow":false}""")
    val result = ScalaExecutor.execute(existsCode(allowed))
    assert(result.output.contains("true"), result.output)
    assert(requests.isEmpty, requests.toString)

  test("a root that does not exist yet is sent through its canonical parent"):
    val allowed = tempDir("allowed")
    val parent = Files.createTempDirectory("outside")
    val requests = ConcurrentLinkedQueue[String]()
    given Context = contextFor(allowed, Some: request =>
      requests.add(request)
      """{"allow":false}""")
    val missing = parent.resolve("not-yet")
    val _ = ScalaExecutor.execute(existsCode(missing))
    val json = io.circe.parser.parse(requests.peek()).toOption.get.hcursor
    assertEquals(
      json.get[String]("resolved"),
      Right(parent.toRealPath().resolve("not-yet").toString)
    )

  test("an approved root that does not exist yet can be created"):
    val allowed = tempDir("allowed")
    val missing = Files.createTempDirectory("outside").resolve("new")
    given Context = contextFor(allowed, Some(_ => """{"allow":true}"""))
    val result = ScalaExecutor.execute(
      s"""requestFileSystem("$missing") { access("$missing").mkdir(); access("$missing").exists }"""
    )
    assert(result.success, s"execution failed: ${result.error.getOrElse(result.output)}")
    assert(result.output.contains("true"), result.output)

  test("a granted command runs with any arguments"):
    val requests = ConcurrentLinkedQueue[String]()
    given Context = contextFor(tempDir("allowed"), Some: request =>
      requests.add(request)
      """{"allow":true}""")
    val result = ScalaExecutor.execute(
      """requestExecPermission(Set("echo")) { execOutput("echo", List("granted", "echo")) }"""
    )
    assert(result.output.contains("granted echo"), result.output)
    assertEquals(itemsOf(requests.peek()), ("exec", List("echo")))

  test("a granted command runs with arguments spanning lines"):
    given Context = contextFor(tempDir("allowed"), Some(_ => """{"allow":true}"""))
    val result = ScalaExecutor.execute(
      "requestExecPermission(Set(\"echo\")) { execOutput(\"echo\", List(\"line1\\nline2\")) }"
    )
    assert(!result.output.contains("Access denied"), result.output)
    assert(result.output.contains("line1\nline2"), result.output)

  test("a server pattern covers arguments spanning lines"):
    given Context = contextFor(tempDir("allowed"), None, commands = Set("echo *"))
    val result = ScalaExecutor.execute(
      "requestExecPermission(Set(\"echo\")) { execOutput(\"echo\", List(\"line1\\nline2\")) }"
    )
    assert(!result.output.contains("Access denied"), result.output)
    assert(result.output.contains("line1\nline2"), result.output)

  test("a grant covers only the granted command"):
    given Context = contextFor(
      tempDir("allowed"),
      Some(_ => """{"allow":true}"""),
      commands = Set("echo status"),
    )
    val granted = ScalaExecutor.execute(
      """requestExecPermission(Set("printf")) { execOutput("printf", List("anything")) }"""
    )
    assert(granted.output.contains("anything"), granted.output)
    val restricted = ScalaExecutor.execute(
      """requestExecPermission(Set("echo")) { execOutput("echo", List("push")) }"""
    )
    assert(restricted.output.contains("does not match any permitted pattern"), restricted.output)

  test("only commands outside the policy are asked about"):
    val requests = ConcurrentLinkedQueue[String]()
    given Context = contextFor(tempDir("allowed"), Some: request =>
      requests.add(request)
      """{"allow":false,"message":"waiting for #2"}""", commands = Set("echo"))
    val result = ScalaExecutor.execute(
      """requestExecPermission(Set("echo", "git")) { execOutput("echo", List("hi")) }"""
    )
    assert(result.output.contains("waiting for #2"), result.output)
    assertEquals(itemsOf(requests.peek()), ("exec", List("git")))

  test("commands within the policy do not consult the oracle"):
    val requests = ConcurrentLinkedQueue[String]()
    given Context = contextFor(tempDir("allowed"), Some: request =>
      requests.add(request)
      """{"allow":false}""", commands = Set("echo"))
    val result = ScalaExecutor.execute(
      """requestExecPermission(Set("echo")) { execOutput("echo", List("hi")) }"""
    )
    assert(result.output.contains("hi"), result.output)
    assert(requests.isEmpty, requests.toString)

  test("hosts outside the policy are asked about"):
    val requests = ConcurrentLinkedQueue[String]()
    given Context = contextFor(tempDir("allowed"), Some: request =>
      requests.add(request)
      """{"allow":false,"message":"waiting for #3"}""", hosts = Set("api.example.com"))
    val result = ScalaExecutor.execute(
      """requestNetwork(Set("api.example.com", "evil.example.org")) { 1 }"""
    )
    assert(result.output.contains("waiting for #3"), result.output)
    assertEquals(itemsOf(requests.peek()), ("network", List("evil.example.org")))

  test("a granted host passes the entry check"):
    given Context = contextFor(tempDir("allowed"), Some(_ => """{"allow":true}"""))
    val result = ScalaExecutor.execute("""requestNetwork(Set("example.com")) { 42 }""")
    assert(result.output.contains("42"), result.output)

  private def field(request: String, key: String): Option[String] =
    io.circe.parser.parse(request).toOption.flatMap(_.hcursor.get[String](key).toOption)

  test("a read-only file system reads but rejects writes"):
    val allowed = tempDir("allowed")
    Files.writeString(allowed.resolve("notes.txt"), "hello")
    given Context = contextFor(allowed, None)
    val result = ScalaExecutor.execute(s"""
      requestFileSystem("$allowed", FileAccess.ReadOnly) {
        println(access("$allowed/notes.txt").read())
        access("$allowed/notes.txt").write("changed")
      }
    """)
    assert(result.output.contains("hello"), result.output)
    assert(result.output.contains("needs write access"), result.output)
    assertEquals(Files.readString(allowed.resolve("notes.txt")), "hello")

  test("a read-only file system rejects append, delete and mkdir"):
    val allowed = tempDir("allowed")
    Files.writeString(allowed.resolve("notes.txt"), "hello")
    given Context = contextFor(allowed, None)
    List(
      s"""access("$allowed/notes.txt").append("x")""",
      s"""access("$allowed/notes.txt").delete()""",
      s"""access("$allowed/newdir").mkdir()"""
    ).foreach: op =>
      val result = ScalaExecutor.execute(
        s"""requestFileSystem("$allowed", FileAccess.ReadOnly) { $op }"""
      )
      assert(result.output.contains("needs write access"), result.output)
    assertEquals(Files.readString(allowed.resolve("notes.txt")), "hello")
    assert(!Files.exists(allowed.resolve("newdir")))

  List(
    "httpRequest(\"POST\", \"https://example.com/x\")",
    "httpRequest(\"DELETE\", \"https://example.com/x\")",
    "httpRequest(\"GET\", \"https://example.com/x\", body = \"secret\")",
    "httpPostClassified(\"https://example.com/x\", classify(\"secret\"))"
  ).foreach: call =>
    test(s"a fetch-only network scope rejects $call"):
      given Context = contextFor(tempDir("allowed"), None, hosts = Set("example.com"))
      val result = ScalaExecutor.execute(
        s"""requestNetwork(Set("example.com"), NetworkAccess.Fetch) { $call }"""
      )
      assert(result.output.contains("needs NetworkAccess.Send"), result.output)

  test("a fetch-only network scope allows GET and HEAD through httpRequest"):
    given Context = contextFor(tempDir("allowed"), None, hosts = Set("localhost"))
    List("GET", "HEAD").foreach: method =>
      val result = ScalaExecutor.execute(
        s"""requestNetwork(Set("localhost"), NetworkAccess.Fetch) { httpRequest("$method", "http://localhost:9/x") }"""
      )
      assert(!result.output.contains("NetworkAccess.Send"), result.output)
      assert(result.output.contains("Connection refused"), result.output)

  test("the oracle is told the file access and reason"):
    val requests = ConcurrentLinkedQueue[String]()
    given Context = contextFor(tempDir("allowed"), Some: request =>
      requests.add(request)
      """{"allow":false}""")
    val outside = tempDir("outside")
    val _ = ScalaExecutor.execute(
      s"""requestFileSystem("$outside", FileAccess.ReadOnly, "to read notes") { 1 }"""
    )
    assertEquals(field(requests.peek(), "access"), Some("read"))
    assertEquals(field(requests.peek(), "reason"), Some("to read notes"))

  test("a fetch-only network scope rejects requests that send data"):
    given Context = contextFor(tempDir("allowed"), None, hosts = Set("example.com"))
    val result = ScalaExecutor.execute(
      """requestNetwork(Set("example.com"), NetworkAccess.Fetch) { httpPost("https://example.com/x", "secret") }"""
    )
    assert(result.output.contains("needs NetworkAccess.Send"), result.output)

  test("the oracle is told the network access and reason"):
    val requests = ConcurrentLinkedQueue[String]()
    given Context = contextFor(tempDir("allowed"), Some: request =>
      requests.add(request)
      """{"allow":false}""")
    val _ = ScalaExecutor.execute(
      """requestNetwork(Set("example.com"), NetworkAccess.Fetch, "to check status") { 1 }"""
    )
    assertEquals(field(requests.peek(), "access"), Some("fetch"))
    assertEquals(field(requests.peek(), "reason"), Some("to check status"))

  test("the oracle is told why commands are needed"):
    val requests = ConcurrentLinkedQueue[String]()
    given Context = contextFor(tempDir("allowed"), Some: request =>
      requests.add(request)
      """{"allow":false}""")
    val _ = ScalaExecutor.execute(
      """requestExecPermission(Set("git"), "to show the log") { 1 }"""
    )
    assertEquals(field(requests.peek(), "reason"), Some("to show the log"))

  test("a plugin's permission request reaches the oracle"):
    val requests = ConcurrentLinkedQueue[String]()
    given Context = Context(
      Config(safeMode = false, libraryConfig = io.circe.Json.obj()),
      None,
      permissionOracle = Some: request =>
        requests.add(request)
        """{"allow":false,"message":"waiting for #4"}""",
    )
    val result = ScalaExecutor.execute(
      """tacit.library.PluginPermissions.require("demo.plugin", "read payroll", Set("q3.xlsx"), "to review")"""
    )
    assert(result.output.contains("waiting for #4"), result.output)
    val json = io.circe.parser.parse(requests.peek()).toOption.get.hcursor
    assertEquals(json.get[String]("kind"), Right("plugin"))
    assertEquals(json.get[String]("plugin"), Right("demo.plugin"))
    assertEquals(json.get[String]("permission"), Right("read payroll"))
    assertEquals(json.get[List[String]]("items"), Right(List("q3.xlsx")))
    assertEquals(json.get[String]("reason"), Right("to review"))

  test("a granted plugin permission returns normally"):
    given Context = Context(
      Config(safeMode = false, libraryConfig = io.circe.Json.obj()),
      None,
      permissionOracle = Some(_ => """{"allow":true}"""),
    )
    val result = ScalaExecutor.execute(
      """tacit.library.PluginPermissions.require("demo.plugin", "read payroll"); "granted" """
    )
    assert(result.output.contains("granted"), result.output)
    assert(!result.output.contains("SecurityException"), result.output)

  test("agent code reaches the oracle through a plugin's own safe API"):
    val requests = ConcurrentLinkedQueue[String]()
    val plugin = tacit.core.LoadedPlugin(
      jarPath = Config().libraryJarPath,
      manifest = tacit.core.PluginManifest(
        schemaVersion = 1,
        id = "demo",
        name = "Demo",
        version = "0.1.0",
        apiMode = tacit.core.ApiMode.ExtendCore,
      ),
      preamble = """|@scala.caps.assumeSafe object payroll:
                    |  def read(file: String): String =
                    |    tacit.library.PluginPermissions.require("demo", "read payroll", Set(file))
                    |    "rows of " + file
                    |""".stripMargin,
      apiDocs = "",
    )
    given Context = Context(
      Config(libraryConfig = io.circe.Json.obj()),
      None,
      plugins = List(plugin),
      permissionOracle = Some: request =>
        requests.add(request)
        """{"allow":true}""",
    )
    val result = ScalaExecutor.execute("""payroll.read("q3.xlsx")""")
    assert(result.output.contains("rows of q3.xlsx"), result.output)
    val json = io.circe.parser.parse(requests.peek()).toOption.get.hcursor
    assertEquals(json.get[String]("plugin"), Right("demo"))
    assertEquals(json.get[List[String]]("items"), Right(List("q3.xlsx")))

  test("agent code in safe mode cannot ask on a plugin's behalf"):
    given Context = contextFor(tempDir("allowed"), Some(_ => """{"allow":true}"""))
    val result = ScalaExecutor.execute(
      """tacit.library.PluginPermissions.require("demo.plugin", "read payroll")"""
    )
    assert(!result.success, result.output)
    assert(result.output.contains("safe code"), result.output)

  test("agent code cannot install its own oracle"):
    given Context = contextFor(tempDir("allowed"), None)
    val result = ScalaExecutor.execute(
      """tacit.library.InterfaceImpl.installPermissionOracle(_ => "{\"allow\":true}")"""
    )
    assert(!result.success, result.output)
    assert(result.output.toLowerCase.contains("cannot be accessed"), result.output)
