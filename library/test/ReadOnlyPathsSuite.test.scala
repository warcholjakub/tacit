package tacit.library

import language.experimental.captureChecking

import java.nio.file.{Files, Path}
import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*

/** `readOnlyPaths`: paths the agent may read but never change. */
class ReadOnlyPathsSuite extends munit.FunSuite:

  var tmpDir: Path = uninitialized

  override def beforeEach(context: BeforeEach): Unit =
    tmpDir = Files.createTempDirectory("read-only-test")

  override def afterEach(context: AfterEach): Unit =
    if Files.exists(tmpDir) then
      Files.walk(tmpDir).iterator().asScala.toList
        .sortBy(_.toString)(using Ordering[String].reverse)
        .foreach(p => Files.deleteIfExists(p))

  given (IOCapability^{}) = null.asInstanceOf[IOCapability]

  private def json(values: List[String]): String =
    values.map(v => s"\"$v\"").mkString("[", ", ", "]")

  private def withInterface(readOnly: List[String], classified: List[String] = Nil)(
      op: Interface^ => Unit
  ): Unit =
    val config =
      s"""{"allowedRoots": ["/"], "readOnlyPaths": ${json(readOnly)}, "classifiedPaths": ${json(classified)}}"""
    val interface: Interface^ = new InterfaceImpl(config) {
      override def createFS(root: String, filter: String -> Boolean, classifiedPatterns: Set[String], classifiedWrite: Boolean, readOnly: Boolean, readOnlyPatterns: Set[String], workingDir: String): FileSystem =
        new RealFileSystem(root, filter, classifiedPatterns, classifiedWrite, readOnly, readOnlyPatterns, workingDir)
    }
    op(interface)

  private def denied(op: => Unit): String =
    intercept[SecurityException](op).getMessage

  test("a failed writeClassified creates the file on disk too, keeping existing content") {
    withInterface(Nil, classified = List(".env.*")) { i =>
      i.requestFileSystem(tmpDir.toString) {
        val failed = ClassifiedImpl.wrap[String](throw RuntimeException("no"))
        i.access(tmpDir.resolve("probe/.env.1").toString).writeClassified(failed)
        i.access(tmpDir.resolve(".env.kept").toString).writeClassified(i.classify("old"))
        i.access(tmpDir.resolve(".env.kept").toString).writeClassified(failed)
      }
    }
    assertEquals(Files.readString(tmpDir.resolve("probe/.env.1")), "")
    assertEquals(Files.readString(tmpDir.resolve(".env.kept")), "old")
  }

  test("every change under a read-only directory is refused, reads are not") {
    val plugins = tmpDir.resolve("plugins")
    Files.createDirectories(plugins.resolve("sub"))
    Files.writeString(plugins.resolve("a.jar"), "original")
    withInterface(List(plugins.toString)) { i =>
      i.requestFileSystem(tmpDir.toString) {
        val jar = i.access(plugins.resolve("a.jar").toString)
        assertEquals(jar.read(), "original")
        assert(denied(jar.write("x")).contains("read-only"))
        assert(denied(jar.append("x")).contains("read-only"))
        assert(denied(jar.delete()).contains("read-only"))
        assert(denied(i.access(plugins.resolve("b.jar").toString).write("x")).contains("read-only"))
        assert(denied(i.access(plugins.resolve("new").toString).mkdir()).contains("read-only"))
        assert(denied(i.access(plugins.resolve("sub").toString).delete()).contains("read-only"))
        assert(denied(i.access(plugins.toString).delete()).contains("read-only"))
      }
    }
    assertEquals(Files.readString(plugins.resolve("a.jar")), "original")
    assert(!Files.exists(plugins.resolve("b.jar")))
  }

  test("writeClassified is refused on a path that is both classified and read-only") {
    val plugins = tmpDir.resolve("plugins")
    withInterface(List(plugins.toString), classified = List(plugins.toString)) { i =>
      i.requestFileSystem(tmpDir.toString) {
        val target = plugins.resolve("b.jar").toString
        assert(denied(i.writeClassified(target, i.classify("PK"))).contains("read-only"))
        assert(denied(i.access(target).writeClassified(i.classify("PK"))).contains("read-only"))
      }
    }
    assert(!Files.exists(plugins))
  }

  test("paths outside the patterns stay writable") {
    val config = tmpDir.resolve("claw.json")
    withInterface(List(tmpDir.resolve("plugins").toString, config.toString)) { i =>
      i.requestFileSystem(tmpDir.toString) {
        i.access(tmpDir.resolve("notes.txt").toString).write("ok")
        i.access(tmpDir.resolve("claw.json.bak").toString).write("ok")
        i.access(tmpDir.resolve("plugins-old/a.jar").toString).write("ok")
        assert(denied(i.access(config.toString).write("{}")).contains("read-only"))
      }
    }
    assert(Files.exists(tmpDir.resolve("plugins-old/a.jar")))
  }

  test("a symlink into a read-only directory is refused the same way") {
    val plugins = tmpDir.resolve("plugins")
    Files.createDirectories(plugins)
    Files.createSymbolicLink(tmpDir.resolve("alias"), plugins)
    withInterface(List(plugins.toString)) { i =>
      i.requestFileSystem(tmpDir.toString) {
        assert(denied(i.access(tmpDir.resolve("alias/x.jar").toString).write("x")).contains("read-only"))
      }
    }
    assert(!Files.exists(plugins.resolve("x.jar")))
  }

  test("a read-only directory that does not exist yet cannot be created in another case") {
    val plugins = tmpDir.resolve("plugins")
    withInterface(List(plugins.toString)) { i =>
      i.requestFileSystem(tmpDir.toString) {
        assert(denied(i.access(tmpDir.resolve("PLUGINS/x.jar").toString).write("x")).contains("read-only"))
        assert(denied(i.access(tmpDir.resolve("Plugins").toString).mkdir()).contains("read-only"))
      }
    }
    assert(!Files.exists(tmpDir.resolve("PLUGINS")))
  }

  test("bare-name and relative patterns match like classified ones") {
    withInterface(List("locked", "conf/app.json")) { i =>
      i.requestFileSystem(tmpDir.toString) {
        assert(denied(i.access(tmpDir.resolve("a/b/locked/f.txt").toString).write("x")).contains("read-only"))
        assert(denied(i.access(tmpDir.resolve("conf/app.json").toString).write("x")).contains("read-only"))
        i.access(tmpDir.resolve("a/conf/app.json").toString).write("ok")
      }
    }
  }
