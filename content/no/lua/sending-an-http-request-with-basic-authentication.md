---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering i Lua er en måte å sikre at bare autentiserte brukere har tilgang til bestemte ressurser. Programmerere gjør dette for å beskytte sensitive data som finnes i webapplikasjoner, APIer og tjenester.

## Slik gjør du:

Først, skal vi installere LuaSocket og LuaSec. Du kan gjøre det ved å bruke `luarocks`:

```Lua
luarocks install luasocket
luarocks install luasec
```

Her er en grunnleggende kode eksempel på hvordan du kan sende en HTTP-forespørsel med grunnleggende autentisering i Lua:

```Lua
http = require("socket.http")
ltn12 = require("ltn12")

url = 'http://example.com'
user = 'brukernavn'
pass = 'passord'

auth = 'Basic ' .. (user .. ':' .. pass):gsub("(%w+)", {["+"] = "%20", ["="] = "%3D"}):gsub(".", function(x) return string.format("%%%02X", x:byte()) end)

headers = { 
  ["Authorization"] = auth
}

body, code = http.request{
  url = url,
  headers = headers,
  sink = ltn12.sink.file(io.stdout)
}

print(code)
```
Merk: `url`, `user`, og `pass` skal byttes ut med din egen url, brukernavn og passord.

## Deep Dive

Å bruke HTTP-forespørsler til autentisering er ikke en ny ide. Det har vært brukt siden opprettelsen av WWW for å beskytte data. Selvfølgelig, det er mange andre metoder og teknikker for å utføre autentisering i forskjellige programmeringsspråk og rammeverk. Alternativer til Basic HTTP-autentisering inkluderer Digest Access Authentication, OAuth, JWT, SSO, etc.

For Lua, et alternativ til å bruke LuaSocket og LuaSec kan være å bruke "http-client" biblioteket. Hvis du bruker en webapplikasjon rammeverk som OpenResty eller Lapis, er de nødvendige funksjonene sannsynligvis allerede innebygd.

## Se Også

- LuaSocket dokumentasjon: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec dokumentasjon: https://github.com/brunoos/luasec/wiki
- HTTP-autentisering: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication