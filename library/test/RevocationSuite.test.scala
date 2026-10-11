package tacit.library

import language.experimental.captureChecking

import caps.unsafe.unsafeAssumePure

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

/** A revoked thread (a timed-out run the server could not stop) must fail
  * every call that acts on the outside world, and get no grants. Each check
  * runs on a fresh thread, since revocation cannot be undone. */
class RevocationSuite extends munit.FunSuite:
  import RevocationSuite.*

  private val interface: Interface^{} = new InterfaceImpl("""{"allowedRoots": ["/"]}""") {
    override def createFS(root: String, filter: String -> Boolean, classifiedPatterns: Set[String], classifiedWrite: Boolean, readOnly: Boolean, readOnlyPatterns: Set[String], workingDir: String): FileSystem =
      new RealFileSystem(root, filter, classifiedPatterns, classifiedWrite, readOnly, readOnlyPatterns, workingDir)
  }.unsafeAssumePure

  import interface.*

  given (IOCapability^{}) = null.asInstanceOf[IOCapability]

  private def revokeMe(): Unit = InterfaceImpl.revoke(Thread.currentThread().nn)

  /** Runs `body` on a fresh thread and returns what it threw, if anything. */
  private def onThread(body: () => Unit): Option[Throwable] =
    val thrown = AtomicReference[Throwable | Null](null)
    val t = Thread(() => try body() catch case e: Exception => thrown.set(e))
    t.start()
    t.join()
    Option(thrown.get())

  private def assertRevoked(thrown: Option[Throwable]): Unit =
    thrown match
      case Some(e: SecurityException) => assert(e.getMessage.nn.contains("revoked"), e.getMessage)
      case other => fail(s"expected a revocation SecurityException, got $other")

  private def tempFile(): Path =
    val f = Files.createTempFile("tacit-revocation", ".txt").nn
    Files.writeString(f, "hello")
    f

  override def beforeAll(): Unit = ensureOracle()

  test("an unrevoked thread is unaffected") {
    val f = tempFile()
    assertEquals(onThread(() => requestFileSystem(f.getParent.toString) { val _ = access(f.toString).read() }), None)
  }

  test("a file entry obtained before revocation fails afterwards") {
    val f = tempFile()
    assertRevoked(onThread(() =>
      requestFileSystem(f.getParent.toString) {
        val entry = access(f.toString)
        revokeMe()
        val _ = entry.read()
      }
    ))
    assertRevoked(onThread(() =>
      requestFileSystem(f.getParent.toString) {
        revokeMe()
        access(f.toString).write("changed")
      }
    ))
    assertEquals(Files.readString(f), "hello")
  }

  test("network, process and LLM calls fail") {
    assertRevoked(onThread(() =>
      requestNetwork(Set("localhost")) {
        revokeMe()
        val _ = WebOps.httpGet("http://localhost:1/", Map.empty, Map.empty)
      }
    ))
    assertRevoked(onThread(() =>
      requestExecPermission(Set("echo")) {
        revokeMe()
        val _ = exec("echo", List("hi"))
      }
    ))
    assertRevoked(onThread(() =>
      revokeMe()
      val _ = new LlmOps(None).chat("hi")
    ))
  }

  test("a revoked thread's permission requests are denied without asking the host") {
    val before = oracleCalls.get
    val answer = AtomicReference[PermissionAnswer | Null](null)
    assertEquals(onThread(() =>
      revokeMe()
      answer.set(InterfaceImpl.askPermission(io.circe.Json.obj()))
    ), None)
    assertEquals(oracleCalls.get, before)
    answer.get() match
      case PermissionAnswer.Deny(Some(message)) => assert(message.contains("revoked"), message)
      case other => fail(s"expected a denial, got $other")
  }

  test("a grant arriving after the thread was revoked is denied") {
    // The host decides while the run times out: it answers allow, but the
    // run was revoked meanwhile.
    answerWith(() =>
      InterfaceImpl.revoke(Thread.currentThread().nn)
      """{"allow":true}"""
    )
    try
      val answer = AtomicReference[PermissionAnswer | Null](null)
      assertEquals(onThread(() => answer.set(InterfaceImpl.askPermission(io.circe.Json.obj()))), None)
      answer.get() match
        case PermissionAnswer.Deny(Some(message)) => assert(message.contains("revoked"), message)
        case other => fail(s"expected a denial, got $other")
    finally answerWith(DefaultAnswer)
  }

object RevocationSuite:
  /** Denies like a missing oracle does, so other suites in this JVM see no
    * difference once it is installed. */
  val DefaultAnswer: () -> String = () => """{"allow":false}"""

  private val answer = AtomicReference[() -> String](DefaultAnswer)
  val oracleCalls = AtomicInteger()

  def answerWith(f: () -> String): Unit = answer.set(f)

  /** The oracle can be installed once per class loader, i.e. once per test
    * run; it delegates to [[answer]]. */
  private lazy val installed: Unit =
    InterfaceImpl.installPermissionOracle: _ =>
      oracleCalls.incrementAndGet()
      answer.get().nn()

  def ensureOracle(): Unit = installed
