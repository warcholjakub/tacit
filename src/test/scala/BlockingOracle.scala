package tacit.executor

import tacit.core.{Context, Config}

import java.nio.file.{Files, Path}
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger

/** A permission oracle that blocks, ignoring interrupts, until [[release]].
  * Agent code waiting in it is stuck outside REPL code, which is where a
  * timed-out run cannot be stopped (as in a long library or JDK call). After
  * [[release]] it allows every request. */
final class BlockingOracle:
  private val gate = CountDownLatch(1)
  val calls = AtomicInteger()

  private val oracle: String => String = _ =>
    calls.incrementAndGet()
    var waiting = true
    while waiting do
      try
        gate.await()
        waiting = false
      catch case _: InterruptedException => ()
    """{"allow":true}"""

  def release(): Unit = gate.countDown()

  /** Safe mode, a fresh allowed root, and this oracle for anything outside it. */
  def context(timeoutMs: Long): Context =
    val root: Path = Files.createTempDirectory("tacit-blocking-oracle").toRealPath()
    Context(
      Config(
        executionTimeoutMs = Some(timeoutMs),
        libraryConfig = io.circe.Json.obj(
          "allowedRoots" -> io.circe.Json.arr(io.circe.Json.fromString(root.toString))
        )
      ),
      None,
      permissionOracle = Some(oracle)
    )

  /** Waits until `n` requests have reached the oracle. */
  def awaitCalls(n: Int): Unit =
    val deadline = System.nanoTime + 30000000000L
    while calls.get < n && System.nanoTime < deadline do Thread.sleep(20)
    assert(calls.get >= n, s"expected $n oracle calls, got ${calls.get}")

object BlockingOracle:
  /** Asks for a root outside the allowed one, so it waits in the oracle. */
  val StuckCode: String = """requestFileSystem("/") { () }"""

  /** Waits until `cond` holds, failing after 30 s. */
  def eventually(cond: => Boolean, what: String): Unit =
    val deadline = System.nanoTime + 30000000000L
    while !cond && System.nanoTime < deadline do Thread.sleep(20)
    assert(cond, s"timed out waiting until $what")
