package tacit.library

/** Lets a TACIT plugin ask the user for a permission the plugin defines,
 *  through the same host permission oracle as `requestFileSystem`,
 *  `requestExecPermission` and `requestNetwork`.
 *
 *  It is deliberately not `@assumeSafe`, so agent code in safe mode cannot
 *  call it and name another plugin. Expose the gated operation through your
 *  plugin's own `@assumeSafe` API (in the plugin JAR or its preamble) that
 *  fixes the plugin id and permission:
 *
 *  ```
 *  @assumeSafe object payroll:
 *    def read(file: String): Table =
 *      PluginPermissions.require("safemode.compreview", "read payroll", Set(file), "to compute the review")
 *      ...
 *  ```
 *
 *  For plugin authors:
 *  - Depend on `lampepfl::tacit-library` at compile time only and do not
 *    bundle it: the REPL's sandbox loads the host's library JAR ahead of
 *    plugin JARs, and that copy holds the oracle.
 *  - The host shows the plugin id, the permission and the items to the user.
 *    Keep `permission` a fixed, human-readable phrase; put what varies (file
 *    names, record ids) in `items`. Items and reason passed through from
 *    agent code are agent-controlled.
 *  - An empty `items` asks for the permission as a whole. Only a grant given
 *    without items covers it, and such a grant does not cover requests that
 *    name items.
 *  - The host may refuse a request without asking: in capybaraclaw, any
 *    blank or non-printable plugin id, permission or item, or a reason over
 *    200 characters.
 *  - Without a host oracle every request is denied.
 *  - The guarantee above needs safe mode; without it agent code can call this
 *    directly with any plugin id. */
object PluginPermissions:
  /** Returns when the user granted the permission for all `items` (earlier
   *  in the session or now); otherwise throws `SecurityException` with the
   *  host's message, e.g. that a request is waiting for the user. */
  def require(
    plugin: String,
    permission: String,
    items: Set[String] = Set.empty,
    reason: String = ""
  ): Unit =
    val request = io.circe.Json.obj(
      "kind" -> io.circe.Json.fromString("plugin"),
      "plugin" -> io.circe.Json.fromString(plugin),
      "permission" -> io.circe.Json.fromString(permission),
      "items" -> io.circe.Json.arr(items.toList.sorted.map(io.circe.Json.fromString)*),
      "reason" -> io.circe.Json.fromString(reason)
    )
    InterfaceImpl.askPermission(request) match
      case PermissionAnswer.Allow => ()
      case PermissionAnswer.Deny(Some(message)) => throw SecurityException(message)
      case PermissionAnswer.Deny(None) =>
        throw SecurityException(
          s"Access denied: plugin '$plugin' needs the user's approval for '$permission'."
        )
