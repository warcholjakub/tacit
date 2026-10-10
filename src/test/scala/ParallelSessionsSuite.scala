import tacit.executor.SessionManager
import tacit.core.{Context, Config}

import java.util.concurrent.{Callable, Executors, TimeUnit}

/** Sessions run in parallel, and each captures only its own output. Safe mode
  * is off so the snippets can sleep, which makes the executions overlap. */
class ParallelSessionsSuite extends munit.FunSuite:

  test("parallel sessions capture only their own println output"):
    given Context = Context(Config(safeMode = false), None)
    val sm = new SessionManager
    val ids = List.fill(4)(sm.createSession().fold(err => fail(s"session creation failed: $err"), identity))
    val pool = Executors.newFixedThreadPool(ids.size)
    try
      val started = System.nanoTime
      val futures = ids.zipWithIndex.map: (id, i) =>
        pool.submit((() =>
          sm.executeInSession(id, s"""for _ <- 1 to 20 do { println("session-$i"); Thread.sleep(50) }""")
        ): Callable[tacit.executor.ExecutionResult])
      val results = futures.map(_.get(60, TimeUnit.SECONDS))
      val elapsedMs = (System.nanoTime - started) / 1000000
      results.zipWithIndex.foreach: (r, i) =>
        assert(r.success, s"session $i failed: ${r.error.getOrElse(r.output)}")
        val lines = r.output.linesIterator.filter(_.startsWith("session-")).toList
        assertEquals(lines, List.fill(20)(s"session-$i"))
      // Each run sleeps for 1s; one at a time they would take over 4s.
      assert(elapsedMs < 3500, s"sessions did not run in parallel: ${elapsedMs}ms")
    finally
      pool.shutdownNow()
      ids.foreach(sm.deleteSession)
