package tacit.library

import language.experimental.captureChecking

import java.nio.file.{Files, Path, Paths}
import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*

/** `workingDir`: what relative paths in agent code resolve against. */
class WorkingDirSuite extends munit.FunSuite:

  var tmpDir: Path = uninitialized

  override def beforeEach(context: BeforeEach): Unit =
    tmpDir = Files.createTempDirectory("working-dir-test").toRealPath()

  override def afterEach(context: AfterEach): Unit =
    if Files.exists(tmpDir) then
      Files.walk(tmpDir).iterator().asScala.toList
        .sortBy(_.toString)(using Ordering[String].reverse)
        .foreach(p => Files.deleteIfExists(p))

  given (IOCapability^{}) = null.asInstanceOf[IOCapability]

  private def withInterface(config: String)(op: Interface^ => Unit): Unit =
    val interface: Interface^ = new InterfaceImpl(config) {}
    op(interface)

  private def inTmp(extra: String = ""): String =
    s"""{"workingDir": "$tmpDir"$extra}"""

  test("relative roots and paths resolve against the working directory") {
    Files.createDirectories(tmpDir.resolve("sub"))
    withInterface(inTmp()) { i =>
      i.requestFileSystem(".") {
        i.access("notes.txt").write("x")
        assertEquals(i.access("notes.txt").path, tmpDir.resolve("notes.txt").toString)
      }
      i.requestFileSystem("sub") {
        i.access("sub/inner.txt").write("y")
      }
    }
    assertEquals(Files.readString(tmpDir.resolve("notes.txt")), "x")
    assertEquals(Files.readString(tmpDir.resolve("sub/inner.txt")), "y")
  }

  test("the working directory is the default bound on roots") {
    withInterface(inTmp()) { i =>
      val error = intercept[SecurityException](
        i.requestFileSystem(InterfaceImpl.currentWorkingDir)(())
      )
      assert(error.getMessage.contains("not within any allowed root"), error.getMessage)
    }
  }

  test("without a working directory, relative paths resolve against the server's") {
    val serverDir = Paths.get(InterfaceImpl.currentWorkingDir).toRealPath()
    withInterface("{}") { i =>
      i.requestFileSystem(".") {
        assertEquals(i.access("x").path, serverDir.resolve("x").toString)
      }
    }
  }

  test("commands run in the working directory unless told otherwise") {
    Files.createDirectories(tmpDir.resolve("sub"))
    withInterface(inTmp(""", "commandPermissions": ["pwd"]""")) { i =>
      i.requestExecPermission(Set("pwd")) {
        assertEquals(i.exec("pwd").stdout.trim, tmpDir.toString)
        assertEquals(
          i.exec("pwd", workingDir = Some("sub")).stdout.trim,
          tmpDir.resolve("sub").toString
        )
      }
    }
  }
