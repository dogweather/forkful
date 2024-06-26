---
date: 2024-01-20 18:02:07.297607-07:00
description: 'Anleitung: Hier ist, wie man es in Lua anstellt.'
lastmod: '2024-03-13T22:44:54.015155-06:00'
model: gpt-4-1106-preview
summary: Hier ist, wie man es in Lua anstellt.
title: HTTP-Anfragen mit Basisauthentifizierung senden
weight: 45
---

## Anleitung:
Hier ist, wie man es in Lua anstellt:

```lua
-- Benötigtes HTTP-Modul importieren
local http = require("socket.http")
local ltn12 = require("ltn12")

-- Benutzername und Passwort festlegen
local username = 'deinBenutzername'
local password = 'deinPasswort'

-- Encoder für Base64
local function basicAuthEncode(user, pass)
  return 'Basic ' .. ((user .. ':' .. pass):gsub("\n", ""):enc())
end

-- Basic Authentication Header
local headers = {}
headers["Authorization"] = basicAuthEncode(username, password)

-- Anfrage und Antwort vorbereiten
local response_body = {}
local res, status = http.request{
  url = 'http://deineurl.de/pfad',
  method = "GET",
  headers = headers,
  sink = ltn12.sink.table(response_body)
}

-- Ergebnis ausgeben
print(res)
print(status)
if res then
    print(table.concat(response_body))
end
```
Im obigen Beispiel sehen mögliche Ausgaben so aus:

```
1
200
{"antwort":"Erfolgreich Authentifiziert"}
```

## Vertiefung:
Historisch gesehen war die Basic Authentication ein früher Mechanismus für HTTP-Authentifizierung, wird aber wegen ihrer Einfachheit noch verwendet. Alternativen sind z.B. OAuth, Token-based Authentifizierung oder Digest Access Authentication, die mehr Sicherheit bieten. Die Implementierung in Lua erfordert das Managen von Headers, und oft wird das Base64-Encoding genutzt, um Benutzername und Passwort zu kodieren.

## Siehe auch:
- LuaSocket Dokumentation: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec für HTTPS-Unterstützung: https://github.com/brunoos/luasec/wiki
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
