package tacit.library

import language.experimental.captureChecking

import com.sun.net.httpserver.{HttpServer, HttpExchange}

import java.net.InetSocketAddress

class WebOpsSuite extends munit.FunSuite:

  var server: HttpServer = scala.compiletime.uninitialized
  var baseUrl: String = scala.compiletime.uninitialized

  // Convenience wrappers for the common no-extra-headers calls.
  private def get(url: String)(using Network): String =
    WebOps.httpGet(url, Map.empty, Map.empty)
  private def post(url: String, body: String, contentType: String)(using Network): String =
    WebOps.httpPost(url, body, contentType, Map.empty, Map.empty)

  override def beforeAll(): Unit =
    server = HttpServer.create(InetSocketAddress(0), 0).nn
    val port = server.getAddress.nn.getPort

    server.createContext("/ok", (ex: HttpExchange) =>
      val body = "hello"
      ex.sendResponseHeaders(200, body.length)
      val os = ex.getResponseBody.nn
      os.write(body.getBytes)
      os.close()
    )

    server.createContext("/echo", (ex: HttpExchange) =>
      val input = String(ex.getRequestBody.nn.readAllBytes())
      ex.sendResponseHeaders(200, input.length)
      val os = ex.getResponseBody.nn
      os.write(input.getBytes)
      os.close()
    )

    server.createContext("/not-found", (ex: HttpExchange) =>
      val body = """{"error": "not found"}"""
      ex.sendResponseHeaders(404, body.length)
      val os = ex.getResponseBody.nn
      os.write(body.getBytes)
      os.close()
    )

    server.createContext("/server-error", (ex: HttpExchange) =>
      val body = "internal server error: something broke"
      ex.sendResponseHeaders(500, body.length)
      val os = ex.getResponseBody.nn
      os.write(body.getBytes)
      os.close()
    )

    // A redirect whose target body must never be returned: following it would
    // let an allowlisted host tunnel the request to a host the allowlist never
    // approved (SSRF). openConnection disables redirects, so httpGet should see
    // the 3xx itself, not the redirect target's content.
    server.createContext("/redirect", (ex: HttpExchange) =>
      ex.getResponseHeaders.nn.set("Location", s"$baseUrl/redirect-target")
      ex.sendResponseHeaders(302, -1)
      ex.close()
    )
    server.createContext("/redirect-target", (ex: HttpExchange) =>
      val body = "REDIRECT TARGET BODY"
      ex.sendResponseHeaders(200, body.length)
      val os = ex.getResponseBody.nn
      os.write(body.getBytes)
      os.close()
    )

    // Echoes the received Authorization header (or "none"), to prove a header
    // value (plain or classified) actually reached the server.
    server.createContext("/whoami", (ex: HttpExchange) =>
      val auth = Option(ex.getRequestHeaders.nn.getFirst("Authorization")).getOrElse("none")
      ex.sendResponseHeaders(200, auth.length)
      val os = ex.getResponseBody.nn
      os.write(auth.getBytes)
      os.close()
    )

    // Drains any request body and echoes the HTTP method, for verb testing.
    server.createContext("/method", (ex: HttpExchange) =>
      ex.getRequestBody.nn.readAllBytes()
      val m = ex.getRequestMethod.nn
      ex.sendResponseHeaders(200, m.length)
      val os = ex.getResponseBody.nn
      os.write(m.getBytes)
      os.close()
    )

    server.start()
    baseUrl = s"http://localhost:$port"

  override def afterAll(): Unit =
    server.stop(0)

  test("httpGet returns response body on success"):
    given Network = NetworkImpl(Set("localhost"))
    val result = get(s"$baseUrl/ok")
    assertEquals(result, "hello")

  test("httpPost sends body and returns response"):
    given Network = NetworkImpl(Set("localhost"))
    val result = post(s"$baseUrl/echo", "ping", "text/plain")
    assertEquals(result, "ping")

  test("httpGet returns error body on 404"):
    given Network = NetworkImpl(Set("localhost"))
    val result = get(s"$baseUrl/not-found")
    assert(result.contains("not found"), s"Expected error body, got: $result")

  test("httpGet returns error body on 500"):
    given Network = NetworkImpl(Set("localhost"))
    val result = get(s"$baseUrl/server-error")
    assert(result.contains("something broke"), s"Expected error body, got: $result")

  test("httpGet rejects host not matching allowlist"):
    given Network = NetworkImpl(Set("example.com"))
    val ex = intercept[SecurityException]:
      get(s"$baseUrl/ok")
    assert(ex.getMessage.nn.contains("does not match any permitted pattern"))

  test("httpGet rejects URL with no host"):
    given Network = NetworkImpl(Set("localhost"))
    val ex = intercept[SecurityException]:
      get("file:///etc/passwd")
    assert(ex.getMessage.nn.contains("no host"))

  test("httpPost returns error body on 404"):
    given Network = NetworkImpl(Set("localhost"))
    val result = post(s"$baseUrl/not-found", "{}", "application/json")
    assert(result.contains("not found"), s"Expected error body, got: $result")

  test("httpGet does not follow redirects (SSRF guard)"):
    given Network = NetworkImpl(Set("localhost"))
    val result = get(s"$baseUrl/redirect")
    assert(!result.contains("REDIRECT TARGET BODY"),
      s"redirect must not be followed, but target body was returned: $result")

  // ── glob matching in the allowlist ──────────────────────────

  test("httpGet allows host matching a glob pattern"):
    given Network = NetworkImpl(Set("local*"))
    val result = get(s"$baseUrl/ok")
    assertEquals(result, "hello")

  test("httpGet rejects host not matching any glob pattern"):
    given Network = NetworkImpl(Set("*.example.com"))
    val ex = intercept[SecurityException]:
      get(s"$baseUrl/ok")
    assert(ex.getMessage.nn.contains("does not match any permitted pattern"))

  // ── headers, methods, and the Classified sink ──────────────────────

  test("httpGet sends a plain custom header"):
    given Network = NetworkImpl(Set("localhost"))
    val result = WebOps.httpGet(s"$baseUrl/whoami", Map("Authorization" -> "Bearer plain-123"), Map.empty)
    assertEquals(result, "Bearer plain-123")

  test("secretHeaders: a Classified header value reaches the host but is never disclosed"):
    given Network = NetworkImpl(Set("localhost"))
    val token = ClassifiedImpl.wrap("Bearer secret-xyz")
    assertEquals(token.toString, "Classified(***)")              // masked to agent code
    val result = WebOps.httpGet(s"$baseUrl/whoami", Map.empty, Map("Authorization" -> token))
    assertEquals(result, "Bearer secret-xyz")                    // yet transmitted to the allowlisted host

  test("secretHeaders: a failed Classified value aborts the request without its exception"):
    given Network = NetworkImpl(Set("localhost"))
    // The computation's exception may carry the secret, e.g. map(v => throw E(v)).
    val bad = ClassifiedImpl.wrap[String](throw RuntimeException("secret-xyz"))
    val e = intercept[IllegalArgumentException]:
      WebOps.httpGet(s"$baseUrl/whoami", Map.empty, Map("Authorization" -> bad))
    assert(!e.getMessage.nn.contains("secret-xyz"), e.getMessage)
    assertEquals(e.getCause, null)

  test("secretHeaders: an invalid Classified value aborts the request without quoting it"):
    given Network = NetworkImpl(Set("localhost"))
    // The JDK's own message for an invalid header value quotes the value.
    val bad = ClassifiedImpl.wrap("Bearer secret-xyz\nX-Injected: 1")
    val e = intercept[IllegalArgumentException]:
      WebOps.httpGet(s"$baseUrl/whoami", Map.empty, Map("Authorization" -> bad))
    assert(!e.getMessage.nn.contains("secret-xyz"), e.getMessage)
    assertEquals(e.getCause, null)

  test("httpRequest issues an arbitrary method and returns status + body"):
    given Network = NetworkImpl(Set("localhost"))
    val resp = WebOps.httpRequest("PUT", s"$baseUrl/method", "payload", Map.empty, Map.empty)
    assertEquals(resp.status, 200)
    assertEquals(resp.body, "PUT")

  test("httpRequest validates the host like the other verbs"):
    given Network = NetworkImpl(Set("example.com"))
    intercept[SecurityException]:
      WebOps.httpRequest("DELETE", s"$baseUrl/method", "", Map.empty, Map.empty)

  test("httpPostClassified returns a Classified response carrying the round-tripped body"):
    given Network = NetworkImpl(Set("localhost"))
    val secret = ClassifiedImpl.wrap("secret-body")
    val reply = WebOps.httpPostClassified(s"$baseUrl/echo", secret, "text/plain", Map.empty, Map.empty)
    assertEquals(reply.toString, "Classified(***)")             // response is masked to agent code
    assertEquals(ClassifiedImpl.unwrap(reply).get, "secret-body") // but round-trips intact

  test("httpPostClassified on a failed body makes no request and stays failed"):
    given Network = NetworkImpl(Set("localhost"))
    val failed = ClassifiedImpl.wrap[String](throw RuntimeException("boom"))
    val reply = WebOps.httpPostClassified(s"$baseUrl/echo", failed, "text/plain", Map.empty, Map.empty)
    assert(ClassifiedImpl.unwrap(reply).isFailure)
