package tacit.library

import language.experimental.captureChecking

import java.net.{URI, HttpURLConnection}
import java.nio.charset.StandardCharsets
import scala.util.{Try, Success, Failure}

object WebOps:
  private val TimeoutMs = 10000

  /** Methods allowed under `NetworkAccess.Fetch`, as long as there is no body. */
  private val FetchMethods = Set("GET", "HEAD")

  /** Parses `url` once and validates its host against `net`. Returns the parsed
   *  URI so the subsequent request dials the same URL the check approved
   *  (parsing twice is a foot-gun: the two parses could disagree). */
  private def validatedUri(url: String)(using net: Network): URI =
    val uri = URI(url)
    val host = uri.getHost
    if host == null then throw SecurityException(s"Invalid URL (no host): $url")
    net.validateHost(host)
    uri

  /** Opens a connection with redirects disabled — otherwise an allowlisted host
   *  replying `302 Location: http://internal/…` would tunnel requests to a host
   *  the allowlist never saw (SSRF). Callers must `disconnect()` the result. */
  private def openConnection(uri: URI): HttpURLConnection =
    val conn = uri.toURL.openConnection().asInstanceOf[HttpURLConnection]
    conn.setInstanceFollowRedirects(false)
    conn.setConnectTimeout(TimeoutMs)
    conn.setReadTimeout(TimeoutMs)
    conn

  /** Applies plain and classified request headers. Classified values are
   *  unwrapped library-internally and written straight to the connection: they
   *  reach the allowlisted host but never flow back to agent code. A classified
   *  value that wraps a failed computation aborts the request (fail closed). */
  private def applyHeaders(
    conn: HttpURLConnection,
    headers: Map[String, String],
    secretHeaders: Map[String, Classified[String]]
  ): Unit =
    headers.foreach((k, v) => conn.setRequestProperty(k, v))
    secretHeaders.foreach: (k, c) =>
      ClassifiedImpl.unwrap(c) match
        case Success(v) => conn.setRequestProperty(k, v)
        case Failure(e) => throw e

  /** Writes a request body and sets its content type. */
  private def writeBody(conn: HttpURLConnection, body: String, contentType: String): Unit =
    conn.setDoOutput(true)
    conn.setRequestProperty("Content-Type", contentType)
    val os = conn.getOutputStream
    try os.write(body.getBytes(StandardCharsets.UTF_8))
    finally os.close()

  /** Reads the response body, falling back to the error stream on HTTP error codes. */
  private def readResponse(conn: HttpURLConnection): String =
    val code = conn.getResponseCode
    val stream = if code >= 400 then conn.getErrorStream else conn.getInputStream
    if stream == null then s"HTTP $code (no response body)"
    else
      try String(stream.readAllBytes(), StandardCharsets.UTF_8)
      finally stream.close()

  def httpGet(
    url: String,
    headers: Map[String, String],
    secretHeaders: Map[String, Classified[String]]
  )(using net: Network): String =
    val conn = openConnection(validatedUri(url))
    try
      conn.setRequestMethod("GET")
      applyHeaders(conn, headers, secretHeaders)
      readResponse(conn)
    finally conn.disconnect()

  def httpPost(
    url: String,
    body: String,
    contentType: String,
    headers: Map[String, String],
    secretHeaders: Map[String, Classified[String]]
  )(using net: Network): String =
    net.requireSend("POST")
    val conn = openConnection(validatedUri(url))
    try
      conn.setRequestMethod("POST")
      applyHeaders(conn, headers, secretHeaders)
      writeBody(conn, body, contentType)
      readResponse(conn)
    finally conn.disconnect()

  def httpRequest(
    method: String,
    url: String,
    body: String,
    headers: Map[String, String],
    secretHeaders: Map[String, Classified[String]]
  )(using net: Network): HttpResponse =
    if !(FetchMethods.contains(method.toUpperCase) && body.isEmpty) then
      net.requireSend(method.toUpperCase)
    val conn = openConnection(validatedUri(url))
    try
      conn.setRequestMethod(method.toUpperCase)
      applyHeaders(conn, headers, secretHeaders)
      if body.nonEmpty then writeBody(conn, body, headers.getOrElse("Content-Type", "application/json"))
      HttpResponse(conn.getResponseCode, readResponse(conn))
    finally conn.disconnect()

  def httpPostClassified(
    url: String,
    body: Classified[String],
    contentType: String,
    headers: Map[String, String],
    secretHeaders: Map[String, Classified[String]]
  )(using net: Network): Classified[String] =
    net.requireSend("POST")
    ClassifiedImpl.unwrap(body) match
      // Run the POST eagerly with the unwrapped secret, then re-classify the
      // outcome (response or failure) so it stays under information-flow control.
      // The call is evaluated to a plain `String` here rather than via `Try { … }`
      // so the resulting `Try` does not capture the `Network` capability (which
      // capture checking would forbid from flowing into a `Classified`).
      case Success(b) =>
        val outcome: Try[String] =
          try Success(httpPost(url, b, contentType, headers, secretHeaders))
          catch case scala.util.control.NonFatal(e) => Failure(e)
        ClassifiedImpl.fromTry(outcome)
      // The body already wraps a failed computation; propagate it untouched
      // without making any request.
      case Failure(_) => body
