# hsyslog-udp
Syslog via UDP in Haskell according to [RFC 5424][rfc].

  [rfc]: https://tools.ietf.org/html/rfc5424

### TODO (in priority order):
- support for `STRUCTURED-DATA`
- optional validation of string identifiers such as `APP-NAME` or `MSGID`
- remove POSIX requirement (only used to determine process ID)
