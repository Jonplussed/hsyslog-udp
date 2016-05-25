# hsyslog-udp
Syslog via UDP in Haskell according to [RFC 5424][rfc].

  [rfc]: https://tools.ietf.org/html/rfc5424

### TODO (in priority order):
- priority-masking
- support for `STRUCTURED-DATA`
- optional validation of string identifiers such as `APP-NAME` or `MSGID`
- verification that a UDP packet was fully transmitted
- tests that prove that a message was actually logged via `withSyslog`
