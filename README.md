# hsyslog-udp
Syslog via UDP in Haskell, supporting message protocols like
[RFC 5424][rfc5424], [RFC 3164][rfc3164], or any arbitrary protocol.

  [rfc5424]: https://tools.ietf.org/html/rfc5424
  [rfc3164]: https://tools.ietf.org/html/rfc3164

### TODO (in priority order):
- support for `STRUCTURED-DATA`
- optional validation of string identifiers such as `APP-NAME` or `MSGID`
- ensure `resolveUdpAddress` is actually a datagram socket.
