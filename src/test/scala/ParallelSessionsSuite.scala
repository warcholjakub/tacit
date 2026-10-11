import tacit.executor.SessionManager
import tacit.core.{Context, Config}

import java.util.concurrent.{Callable, Executors, TimeUnit}

/** Sessions run in parallel, and each captures only its own output. Safe mode
  * is off so the snippets can sleep, which makes the executions overlap. */
class ParallelSessionsSuite extends munit.FunSuite:
  private def session(sm: SessionManager): String =
    sm.createSession().fold(err => fail(s"session creation failed: $err"), identity)

  test("parallel sessions capture only their own println output"):
    given Context = Context(Config(safeMode = false), None)
    val sm = new SessionManager
    val ids = List.fill(4)(sm.createSession().fold(err => fail(s"session creation failed: $err"), identity))
    val pool = Executors.newFixedThreadPool(ids.size)
    try
      val started = System.nanoTime
      val futures = ids.zipWithIndex.map: (id, i) =>
        pool.submit((() =>
          sm.executeInSession(id, s"""for _ <- 1 to 40 do { println("session-$i"); Thread.sleep(50) }""")
        ): Callable[tacit.executor.ExecutionResult])
      val results = futures.map(_.get(60, TimeUnit.SECONDS))
      val elapsedMs = (System.nanoTime - started) / 1000000
      results.zipWithIndex.foreach: (r, i) =>
        assert(r.success, s"session $i failed: ${r.error.getOrElse(r.output)}")
        val lines = r.output.linesIterator.filter(_.startsWith("session-")).toList
        assertEquals(lines, List.fill(40)(s"session-$i"))
      // Each run sleeps for 2s; one at a time they would take over 8s.
      assert(elapsedMs < 6000, s"sessions did not run in parallel: ${elapsedMs}ms")
    finally
      pool.shutdownNow()
      ids.foreach(sm.deleteSession)

  test("half a surrogate pair does not carry over into another session"):
    given Context = Context(Config(safeMode = false), None)
    val sm = new SessionManager
    val (a, b) = (session(sm), session(sm))
    try
      val first = sm.executeInSession(a, "print(\"secret-A \\uD83D\")")
      assert(first.success, s"got: ${first.error.getOrElse(first.output)}")
      val second = sm.executeInSession(b, "print(\"\\uDE00 hello\")")
      assert(second.output.contains("hello"), second.output)
      assert(!second.output.contains("\uD83D"), s"session A's half pair reached B: ${second.output}")
    finally
      sm.deleteSession(a)
      sm.deleteSession(b)

  test("host output during a run stays out of the capture, and stderr is captured"):
    given Context = Context(Config(safeMode = false), None)
    val sm = new SessionManager
    val id = session(sm)
    val host = Thread: () =>
      for _ <- 1 to 10 do
        System.out.println("host-out")
        System.err.println("host-err")
        Thread.sleep(50)
    try
      host.start()
      val r = sm.executeInSession(id,
        // printStackTrace writes to System.err, as library and JDK code do.
        """for _ <- 1 to 10 do { println("session-out"); RuntimeException("session-err").printStackTrace(); Thread.sleep(50) }""")
      host.join()
      assert(r.success, s"got: ${r.error.getOrElse(r.output)}")
      assertEquals(r.output.linesIterator.count(_ == "session-out"), 10, r.output)
      assertEquals(r.output.linesIterator.count(_.contains("RuntimeException: session-err")), 10, r.output)
      assert(!r.output.contains("host-"), s"host output reached the capture: ${r.output}")
    finally sm.deleteSession(id)
