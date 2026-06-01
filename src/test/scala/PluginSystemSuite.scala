package tacit

import tacit.core.*
import tacit.executor.ManagedRepl
import tacit.mcp.computeInterfaceReference

import java.io.FileOutputStream
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

class PluginSystemSuite extends munit.FunSuite:

  private val fakeCoreJar: Path = Files.createTempFile("fake-tacit-core", ".jar")
  private val corePath: String = fakeCoreJar.toAbsolutePath.toString

  override def afterAll(): Unit =
    Files.deleteIfExists(fakeCoreJar)

  /** Write a JAR file at `dest` whose top-level entries are the given
    * `name → content` pairs, encoded as UTF-8.
    */
  private def writeJar(dest: Path, entries: Map[String, String]): Unit =
    val out = new JarOutputStream(new FileOutputStream(dest.toFile))
    try
      entries.foreach: (name, content) =>
        out.putNextEntry(new JarEntry(name))
        out.write(content.getBytes("UTF-8"))
        out.closeEntry()
    finally out.close()

  private def withJar[T](entries: Map[String, String])(f: Path => T): T =
    val jar = Files.createTempFile("plugin", ".jar")
    writeJar(jar, entries)
    try f(jar) finally Files.deleteIfExists(jar)

  private def quietly[T](f: => T): T =
    val origErr = System.err
    System.setErr(new java.io.PrintStream(java.io.OutputStream.nullOutputStream()))
    try f finally System.setErr(origErr)

  private def manifestJson(
    id: String = "demo.test",
    name: String = "Test Plugin",
    version: String = "0.1.0",
    apiMode: String = "replace-core",
    extraFields: String = "",
  ): String =
    s"""{
       |  "schemaVersion": 1,
       |  "id": "$id",
       |  "name": "$name",
       |  "version": "$version",
       |  "apiMode": "$apiMode"$extraFields
       |}""".stripMargin

  private def pluginEntries(
    id: String = "demo.test",
    apiMode: String = "replace-core",
    preamble: String = "// plugin preamble",
    apiDocs: String = "# Plugin API\n\nUse `demoMethod()`.",
    extraFields: String = "",
  ): Map[String, String] = Map(
    "tacit-plugin.json" -> manifestJson(id = id, apiMode = apiMode, extraFields = extraFields),
    "preamble.scala"    -> preamble,
    "api-docs.md"       -> apiDocs,
  )

  private def loadedPlugin(
    jarPath: String = "/fake/plugin.jar",
    id: String = "demo.test",
    name: String = "Test Plugin",
    version: String = "0.1.0",
    apiMode: ApiMode = ApiMode.ReplaceCore,
    preamble: String = "// plugin preamble",
    apiDocs: String = "# Plugin API",
    domain: Option[String] = None,
    description: Option[String] = None,
  ): LoadedPlugin =
    LoadedPlugin(
      jarPath = jarPath,
      manifest = PluginManifest(
        schemaVersion = 1,
        id = id,
        name = name,
        version = version,
        apiMode = apiMode,
        domain = domain,
        description = description,
      ),
      preamble = preamble,
      apiDocs = apiDocs,
    )

  // ── Config parsing ─────────────────────────────────────────────

  test("default pluginJars is empty"):
    assertEquals(Config().pluginJars, List.empty[String])

  test("--plugin (single) populates pluginJars"):
    withJar(pluginEntries()): jar =>
      val cfg = Config.parseCliArgs(Array(
        "--library-jar", corePath,
        "--plugin", jar.toAbsolutePath.toString,
      )).get
      assertEquals(cfg.pluginJars, List(jar.toAbsolutePath.toString))

  test("--plugin repeated accumulates in order"):
    withJar(pluginEntries()): a =>
      withJar(pluginEntries()): b =>
        val cfg = Config.parseCliArgs(Array(
          "--library-jar", corePath,
          "--plugin", a.toAbsolutePath.toString,
          "--plugin", b.toAbsolutePath.toString,
        )).get
        assertEquals(
          cfg.pluginJars,
          List(a.toAbsolutePath.toString, b.toAbsolutePath.toString),
        )

  test("nonexistent --plugin returns None"):
    val cfg = quietly:
      Config.parseCliArgs(Array(
        "--library-jar", corePath,
        "--plugin", "/nonexistent/missing.jar",
      ))
    assertEquals(cfg, None)

  test("JSON config sets pluginJars"):
    withJar(pluginEntries()): jar =>
      val configFile = Files.createTempFile("test-config", ".json")
      val jarStr = jar.toAbsolutePath.toString
      Files.writeString(configFile, s"""
        {
          "libraryJarPath": "$corePath",
          "pluginJars": ["$jarStr"]
        }
      """)
      try
        val cfg = Config.parseCliArgs(Array("--config", configFile.toAbsolutePath.toString)).get
        assertEquals(cfg.pluginJars, List(jarStr))
      finally Files.deleteIfExists(configFile)

  // ── PluginLoader ───────────────────────────────────────────────

  test("PluginLoader.load succeeds with default entrypoints"):
    withJar(pluginEntries()): jar =>
      val res = PluginLoader.load(jar.toAbsolutePath.toString)
      assert(res.isRight, res.toString)
      val plug = res.toOption.get
      assertEquals(plug.manifest.id, "demo.test")
      assertEquals(plug.manifest.apiMode, ApiMode.ReplaceCore)
      assertEquals(plug.preamble, "// plugin preamble")
      assert(plug.apiDocs.contains("demoMethod"))

  test("PluginLoader.load fails if tacit-plugin.json missing"):
    withJar(Map(
      "preamble.scala" -> "// noop",
      "api-docs.md"    -> "# API",
    )): jar =>
      val res = PluginLoader.load(jar.toAbsolutePath.toString)
      assert(res.isLeft)
      assert(res.swap.toOption.get.contains("tacit-plugin.json"))

  test("PluginLoader.load fails for unsupported apiMode"):
    withJar(pluginEntries(apiMode = "frobnicate")): jar =>
      val res = PluginLoader.load(jar.toAbsolutePath.toString)
      assert(res.isLeft)
      assert(res.swap.toOption.get.contains("Unsupported apiMode"))

  test("PluginLoader.load fails if preamble.scala missing"):
    withJar(Map(
      "tacit-plugin.json" -> manifestJson(),
      "api-docs.md"       -> "# API",
    )): jar =>
      val res = PluginLoader.load(jar.toAbsolutePath.toString)
      assert(res.isLeft)
      assert(res.swap.toOption.get.contains("preamble.scala"))

  test("PluginLoader.load fails if api-docs.md missing"):
    withJar(Map(
      "tacit-plugin.json" -> manifestJson(),
      "preamble.scala"    -> "// noop",
    )): jar =>
      val res = PluginLoader.load(jar.toAbsolutePath.toString)
      assert(res.isLeft)
      assert(res.swap.toOption.get.contains("api-docs.md"))

  test("PluginLoader.load rejects unsupported schemaVersion"):
    val badManifest = """{
      "schemaVersion": 2,
      "id": "demo.test",
      "name": "Test",
      "version": "0.1.0",
      "apiMode": "replace-core"
    }"""
    withJar(Map(
      "tacit-plugin.json" -> badManifest,
      "preamble.scala"    -> "// noop",
      "api-docs.md"       -> "# API",
    )): jar =>
      val res = PluginLoader.load(jar.toAbsolutePath.toString)
      assert(res.isLeft)
      assert(res.swap.toOption.get.contains("schemaVersion"))

  test("PluginLoader.load rejects empty id"):
    withJar(pluginEntries() + ("tacit-plugin.json" -> manifestJson(id = ""))): jar =>
      val res = PluginLoader.load(jar.toAbsolutePath.toString)
      assert(res.isLeft)
      assert(res.swap.toOption.get.contains("id"))

  test("PluginLoader.load honors custom entrypoint paths"):
    val customManifest = """{
      "schemaVersion": 1,
      "id": "demo.test",
      "name": "Test",
      "version": "0.1.0",
      "apiMode": "replace-core",
      "entrypoint": { "preamble": "boot.scala", "apiDocs": "docs.md" }
    }"""
    withJar(Map(
      "tacit-plugin.json" -> customManifest,
      "boot.scala"        -> "// custom preamble",
      "docs.md"           -> "# Custom docs",
    )): jar =>
      val res = PluginLoader.load(jar.toAbsolutePath.toString)
      assert(res.isRight, res.toString)
      val plug = res.toOption.get
      assertEquals(plug.preamble, "// custom preamble")
      assert(plug.apiDocs.contains("Custom docs"))

  test("PluginLoader.loadAll preserves order"):
    withJar(pluginEntries(id = "demo.a", preamble = "// A")): a =>
      withJar(pluginEntries(id = "demo.b", preamble = "// B")): b =>
        val res = PluginLoader.loadAll(
          List(a.toAbsolutePath.toString, b.toAbsolutePath.toString)
        )
        assert(res.isRight)
        val preambles = res.toOption.get.map(_.preamble)
        assertEquals(preambles, List("// A", "// B"))

  test("PluginLoader.loadAll rejects mixed extend-core and replace-core"):
    withJar(pluginEntries(id = "demo.a", apiMode = "extend-core")): a =>
      withJar(pluginEntries(id = "demo.b", apiMode = "replace-core")): b =>
        val res = PluginLoader.loadAll(
          List(a.toAbsolutePath.toString, b.toAbsolutePath.toString)
        )
        assert(res.isLeft)
        assert(res.swap.toOption.get.contains("mix"))

  test("PluginLoader.loadAll accepts two replace-core plugins"):
    withJar(pluginEntries(id = "demo.a", apiMode = "replace-core")): a =>
      withJar(pluginEntries(id = "demo.b", apiMode = "replace-core")): b =>
        val res = PluginLoader.loadAll(
          List(a.toAbsolutePath.toString, b.toAbsolutePath.toString)
        )
        assert(res.isRight)

  // ── ManagedRepl.libraryPreamble ────────────────────────────────

  private def ctxFor(plugins: List[LoadedPlugin]): Context =
    Context(
      config = Config(libraryJarPath = corePath),
      recorder = None,
      plugins = plugins,
    )

  test("libraryPreamble with no plugins emits core preamble"):
    given Context = ctxFor(Nil)
    val preamble = ManagedRepl.libraryPreamble
    assert(preamble.contains("import tacit.library.*"))

  test("libraryPreamble with one extend-core plugin includes core + plugin"):
    given Context = ctxFor(List(loadedPlugin(
      apiMode = ApiMode.ExtendCore,
      preamble = "import demo.plugin.*",
    )))
    val preamble = ManagedRepl.libraryPreamble
    assert(preamble.contains("import tacit.library.*"))
    assert(preamble.contains("import demo.plugin.*"))
    assert(
      preamble.indexOf("import tacit.library.*") < preamble.indexOf("import demo.plugin.*")
    )

  test("libraryPreamble with one replace-core plugin omits core"):
    given Context = ctxFor(List(loadedPlugin(
      apiMode = ApiMode.ReplaceCore,
      preamble = "import safemode.lib.*",
    )))
    val preamble = ManagedRepl.libraryPreamble
    assert(!preamble.contains("import tacit.library.*"))
    assert(preamble.contains("import safemode.lib.*"))

  test("libraryPreamble with two replace-core plugins concatenates in order, no core"):
    given Context = ctxFor(List(
      loadedPlugin(apiMode = ApiMode.ReplaceCore, preamble = "import safemode.lib.*"),
      loadedPlugin(apiMode = ApiMode.ReplaceCore, preamble = "import safemode.excel.*"),
    ))
    val preamble = ManagedRepl.libraryPreamble
    assert(!preamble.contains("import tacit.library.*"))
    assert(preamble.contains("import safemode.lib.*"))
    assert(preamble.contains("import safemode.excel.*"))
    assert(
      preamble.indexOf("import safemode.lib.*") < preamble.indexOf("import safemode.excel.*")
    )

  // ── McpServer.computeInterfaceReference ────────────────────────

  test("show_interface with no plugins contains core Interface.scala docs"):
    given Context = ctxFor(Nil)
    val out = computeInterfaceReference
    // The core preamble warning is always present.
    assert(out.contains("You must only use the provided interface"))
    // Either the real Interface.scala or a clear fallback marker — both are
    // acceptable, but neither should mention plugin summary.
    assert(!out.contains("# Loaded TACIT plugins"))

  test("show_interface with extend-core plugin includes core docs and plugin docs"):
    given Context = ctxFor(List(loadedPlugin(
      apiMode = ApiMode.ExtendCore,
      name = "Demo Extend",
      apiDocs = "# Demo Extend API\n\nUse `extend()`.",
    )))
    val out = computeInterfaceReference
    assert(out.contains("# Loaded TACIT plugins"))
    assert(out.contains("Mode: extend-core"))
    assert(out.contains("Demo Extend"))
    assert(out.contains("Use `extend()`"))
    // Core docs presence: either the scaladoc block markers from
    // coreInterfaceReference or the fallback string.
    assert(out.contains("```scala") || out.contains("Interface.scala source not found"))

  test("show_interface with replace-core plugin omits core docs"):
    given Context = ctxFor(List(loadedPlugin(
      apiMode = ApiMode.ReplaceCore,
      name = "Demo Replace",
      apiDocs = "# Demo Replace API\n\nUse `replace()`.",
    )))
    val out = computeInterfaceReference
    assert(out.contains("# Loaded TACIT plugins"))
    assert(out.contains("Mode: replace-core"))
    assert(out.contains("Demo Replace"))
    assert(out.contains("Use `replace()`"))
    // No core scaladoc block: only plugin api-docs may contain ```scala fences
    // (the docs we set above don't), so absence of the marker confirms no
    // coreInterfaceReference was emitted.
    assert(!out.contains("```scala"))
    assert(!out.contains("Interface.scala source not found"))

  test("show_interface with two replace-core plugins lists both in order"):
    given Context = ctxFor(List(
      loadedPlugin(apiMode = ApiMode.ReplaceCore, name = "Alpha", apiDocs = "# Alpha docs"),
      loadedPlugin(apiMode = ApiMode.ReplaceCore, name = "Beta",  apiDocs = "# Beta docs"),
    ))
    val out = computeInterfaceReference
    assert(out.contains("Alpha"))
    assert(out.contains("Beta"))
    assert(out.contains("Alpha docs"))
    assert(out.contains("Beta docs"))
    assert(out.indexOf("Alpha docs") < out.indexOf("Beta docs"))

  // ── PluginLoader.scanDir ───────────────────────────────────────

  private def withDir[T](f: Path => T): T =
    val dir = Files.createTempDirectory("plugin-scan")
    try f(dir) finally deleteRecursively(dir)

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      if Files.isDirectory(path) then
        val children = Files.list(path)
        try children.forEach(deleteRecursively)
        finally children.close()
      Files.deleteIfExists(path)

  test("scanDir on empty folder returns empty list"):
    withDir: dir =>
      val res = PluginLoader.scanDir(dir.toAbsolutePath.toString)
      assertEquals(res, Right(Nil))

  test("scanDir ignores non-jar files"):
    withDir: dir =>
      Files.writeString(dir.resolve("notes.txt"), "hello")
      Files.writeString(dir.resolve("plugin.JAR"), "ignored")  // case-sensitive
      val res = PluginLoader.scanDir(dir.toAbsolutePath.toString)
      assertEquals(res, Right(Nil))

  test("scanDir returns jars sorted alphabetically by file name"):
    withDir: dir =>
      writeJar(dir.resolve("z-plugin.jar"), pluginEntries())
      writeJar(dir.resolve("a-plugin.jar"), pluginEntries())
      writeJar(dir.resolve("m-plugin.jar"), pluginEntries())
      val res = PluginLoader.scanDir(dir.toAbsolutePath.toString)
      assert(res.isRight)
      val names = res.toOption.get.map(p => java.io.File(p).getName)
      assertEquals(names, List("a-plugin.jar", "m-plugin.jar", "z-plugin.jar"))

  test("scanDir fails if path does not exist"):
    val res = PluginLoader.scanDir("/nonexistent/plugin/dir")
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("not found"))

  test("scanDir fails if path exists but is a file"):
    withJar(pluginEntries()): jar =>
      val res = PluginLoader.scanDir(jar.toAbsolutePath.toString)
      assert(res.isLeft)
      assert(res.swap.toOption.get.contains("not a directory"))

  // ── PluginLoader.loadAll(jars, scanDirs) ───────────────────────

  test("loadAll loads plugins from scan dir"):
    withDir: dir =>
      writeJar(dir.resolve("excel.jar"), pluginEntries(preamble = "// excel"))
      val res = PluginLoader.loadAll(Nil, List(dir.toAbsolutePath.toString))
      assert(res.isRight, res.toString)
      val plugs = res.toOption.get
      assertEquals(plugs.length, 1)
      assertEquals(plugs.head.preamble, "// excel")

  test("loadAll unions explicit jars and scan dirs, explicit first"):
    withJar(pluginEntries(id = "demo.explicit", preamble = "// explicit")): explicitJar =>
      withDir: dir =>
        writeJar(
          dir.resolve("scanned.jar"),
          pluginEntries(id = "demo.scanned", preamble = "// scanned"),
        )
        val res = PluginLoader.loadAll(
          List(explicitJar.toAbsolutePath.toString),
          List(dir.toAbsolutePath.toString),
        )
        assert(res.isRight)
        val preambles = res.toOption.get.map(_.preamble)
        assertEquals(preambles, List("// explicit", "// scanned"))

  test("loadAll deduplicates when same absolute path appears in both sources"):
    withDir: dir =>
      val jar = dir.resolve("excel.jar")
      writeJar(jar, pluginEntries(preamble = "// only"))
      val res = PluginLoader.loadAll(
        List(jar.toAbsolutePath.toString),
        List(dir.toAbsolutePath.toString),
      )
      assert(res.isRight)
      assertEquals(res.toOption.get.length, 1)

  test("loadAll rejects mixed apiMode across scan dir contents"):
    withDir: dir =>
      writeJar(
        dir.resolve("a.jar"),
        pluginEntries(id = "demo.a", apiMode = "extend-core"),
      )
      writeJar(
        dir.resolve("b.jar"),
        pluginEntries(id = "demo.b", apiMode = "replace-core"),
      )
      val res = PluginLoader.loadAll(Nil, List(dir.toAbsolutePath.toString))
      assert(res.isLeft)
      assert(res.swap.toOption.get.contains("mix"))

  // ── CLI --plugin-dir parsing ──────────────────────────────────

  test("default pluginScanDirs is empty"):
    assertEquals(Config().pluginScanDirs, List.empty[String])

  test("--plugin-dir (single) populates pluginScanDirs"):
    withDir: dir =>
      val cfg = Config.parseCliArgs(Array(
        "--library-jar", corePath,
        "--plugin-dir", dir.toAbsolutePath.toString,
      )).get
      assertEquals(cfg.pluginScanDirs, List(dir.toAbsolutePath.toString))

  test("--plugin-dir repeated accumulates in order"):
    withDir: a =>
      withDir: b =>
        val cfg = Config.parseCliArgs(Array(
          "--library-jar", corePath,
          "--plugin-dir", a.toAbsolutePath.toString,
          "--plugin-dir", b.toAbsolutePath.toString,
        )).get
        assertEquals(
          cfg.pluginScanDirs,
          List(a.toAbsolutePath.toString, b.toAbsolutePath.toString),
        )

  test("nonexistent --plugin-dir returns None"):
    val cfg = quietly:
      Config.parseCliArgs(Array(
        "--library-jar", corePath,
        "--plugin-dir", "/nonexistent/plugin/dir",
      ))
    assertEquals(cfg, None)

  test("--plugin-dir pointing at a file returns None"):
    withJar(pluginEntries()): jar =>
      val cfg = quietly:
        Config.parseCliArgs(Array(
          "--library-jar", corePath,
          "--plugin-dir", jar.toAbsolutePath.toString,
        ))
      assertEquals(cfg, None)

  test("JSON config sets pluginScanDirs"):
    withDir: dir =>
      val configFile = Files.createTempFile("test-config", ".json")
      val dirStr = dir.toAbsolutePath.toString
      Files.writeString(configFile, s"""
        {
          "libraryJarPath": "$corePath",
          "pluginScanDirs": ["$dirStr"]
        }
      """)
      try
        val cfg = Config.parseCliArgs(Array("--config", configFile.toAbsolutePath.toString)).get
        assertEquals(cfg.pluginScanDirs, List(dirStr))
      finally Files.deleteIfExists(configFile)

  // ── PluginManifest.requires ────────────────────────────────────

  test("manifest decoder defaults requires to Nil when absent"):
    val json = manifestJson()
    val res = io.circe.parser.decode[PluginManifest](json)
    assert(res.isRight, res.toString)
    assertEquals(res.toOption.get.requires, List.empty[String])

  test("manifest decoder honors requires when present"):
    val json = manifestJson(
      extraFields = """, "requires": ["safemode.capabilities", "safemode.other"]""",
    )
    val res = io.circe.parser.decode[PluginManifest](json)
    assert(res.isRight, res.toString)
    assertEquals(
      res.toOption.get.requires,
      List("safemode.capabilities", "safemode.other"),
    )

  // ── PluginLoader.validateAndOrder ──────────────────────────────

  private def withRequires(p: LoadedPlugin, requires: List[String]): LoadedPlugin =
    p.copy(manifest = p.manifest.copy(requires = requires))

  test("validateAndOrder rejects duplicate plugin IDs"):
    val a1 = loadedPlugin(jarPath = "/p/a1.jar", id = "dup.id", name = "First")
    val a2 = loadedPlugin(jarPath = "/p/a2.jar", id = "dup.id", name = "Second")
    val res = PluginLoader.validateAndOrder(List(a1, a2))
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("Duplicate"))
    assert(res.swap.toOption.get.contains("dup.id"))

  test("validateAndOrder rejects self-dependency"):
    val p = withRequires(loadedPlugin(id = "self.me"), List("self.me"))
    val res = PluginLoader.validateAndOrder(List(p))
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("self"))
    assert(res.swap.toOption.get.contains("self.me"))

  test("validateAndOrder rejects missing required plugin"):
    val p = withRequires(loadedPlugin(id = "needs.dep"), List("ghost.lib"))
    val res = PluginLoader.validateAndOrder(List(p))
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("ghost.lib"))
    assert(res.swap.toOption.get.contains("needs.dep"))

  test("validateAndOrder rejects direct cycle"):
    val a = withRequires(loadedPlugin(jarPath = "/p/a.jar", id = "a"), List("b"))
    val b = withRequires(loadedPlugin(jarPath = "/p/b.jar", id = "b"), List("a"))
    val res = PluginLoader.validateAndOrder(List(a, b))
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("cycle"))
    assert(res.swap.toOption.get.contains("a"))
    assert(res.swap.toOption.get.contains("b"))

  test("validateAndOrder rejects transitive cycle"):
    val a = withRequires(loadedPlugin(jarPath = "/p/a.jar", id = "a"), List("b"))
    val b = withRequires(loadedPlugin(jarPath = "/p/b.jar", id = "b"), List("c"))
    val c = withRequires(loadedPlugin(jarPath = "/p/c.jar", id = "c"), List("a"))
    val res = PluginLoader.validateAndOrder(List(a, b, c))
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("cycle"))

  test("validateAndOrder rejects mixed extend-core and replace-core"):
    val ext = loadedPlugin(jarPath = "/p/a.jar", id = "a", apiMode = ApiMode.ExtendCore)
    val rep = loadedPlugin(jarPath = "/p/b.jar", id = "b", apiMode = ApiMode.ReplaceCore)
    val res = PluginLoader.validateAndOrder(List(ext, rep))
    assert(res.isLeft)
    assert(res.swap.toOption.get.contains("mix"))

  test("validateAndOrder topologically orders simple dependency"):
    val cap = loadedPlugin(jarPath = "/p/cap.jar", id = "cap")
    val dep = withRequires(
      loadedPlugin(jarPath = "/p/dep.jar", id = "dep"),
      List("cap"),
    )
    // Pass in reverse order to prove sort actually reorders.
    val res = PluginLoader.validateAndOrder(List(dep, cap))
    assert(res.isRight, res.toString)
    assertEquals(res.toOption.get.map(_.manifest.id), List("cap", "dep"))

  test("validateAndOrder topologically orders transitive chain"):
    val a = loadedPlugin(jarPath = "/p/a.jar", id = "a")
    val b = withRequires(loadedPlugin(jarPath = "/p/b.jar", id = "b"), List("a"))
    val c = withRequires(loadedPlugin(jarPath = "/p/c.jar", id = "c"), List("b"))
    // Pass in reverse order.
    val res = PluginLoader.validateAndOrder(List(c, b, a))
    assert(res.isRight, res.toString)
    assertEquals(res.toOption.get.map(_.manifest.id), List("a", "b", "c"))

  test("validateAndOrder preserves input order within the same dependency depth"):
    val a = loadedPlugin(jarPath = "/p/a.jar", id = "a")
    val b = withRequires(loadedPlugin(jarPath = "/p/b.jar", id = "b"), List("a"))
    val c = withRequires(loadedPlugin(jarPath = "/p/c.jar", id = "c"), List("a"))
    // b and c are both at depth 1; input gave us b before c, so result keeps that.
    val res = PluginLoader.validateAndOrder(List(b, c, a))
    assert(res.isRight)
    assertEquals(res.toOption.get.map(_.manifest.id), List("a", "b", "c"))
    // Reversing the depth-1 input swaps their order in the output.
    val swapped = PluginLoader.validateAndOrder(List(c, b, a))
    assert(swapped.isRight)
    assertEquals(swapped.toOption.get.map(_.manifest.id), List("a", "c", "b"))

  test("loadAll returns plugins in topological order"):
    withDir: dir =>
      // Filename order: a.jar, m.jar, z.jar; dependency order: m → a → z (none).
      // a requires m. So expected output order is [m, z, a] (or some valid topo).
      // Use explicit deps to make the test deterministic: z is independent,
      // a requires m, m has no deps. Expected: m and z at depth 0, a at depth 1.
      // Tie-break alphabetical jarPath: m before z. Final: [m, z, a].
      writeJar(
        dir.resolve("a.jar"),
        Map(
          "tacit-plugin.json" -> manifestJson(
            id = "plugin.a",
            extraFields = """, "requires": ["plugin.m"]""",
          ),
          "preamble.scala" -> "// a",
          "api-docs.md"    -> "# a",
        ),
      )
      writeJar(
        dir.resolve("m.jar"),
        Map(
          "tacit-plugin.json" -> manifestJson(id = "plugin.m"),
          "preamble.scala"    -> "// m",
          "api-docs.md"       -> "# m",
        ),
      )
      writeJar(
        dir.resolve("z.jar"),
        Map(
          "tacit-plugin.json" -> manifestJson(id = "plugin.z"),
          "preamble.scala"    -> "// z",
          "api-docs.md"       -> "# z",
        ),
      )
      val res = PluginLoader.loadAll(Nil, List(dir.toAbsolutePath.toString))
      assert(res.isRight, res.toString)
      assertEquals(
        res.toOption.get.map(_.manifest.id),
        List("plugin.m", "plugin.z", "plugin.a"),
      )
