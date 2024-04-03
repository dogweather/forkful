---
date: 2024-01-20 18:02:23.093696-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering betyr\
  \ \xE5 inkludere brukernavn og passord i foresp\xF8rselen for \xE5 f\xE5 tilgang\
  \ til beskyttede\u2026"
lastmod: '2024-03-13T22:44:40.929792-06:00'
model: gpt-4-1106-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering betyr\
  \ \xE5 inkludere brukernavn og passord i foresp\xF8rselen for \xE5 f\xE5 tilgang\
  \ til beskyttede ressurser."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering betyr å inkludere brukernavn og passord i forespørselen for å få tilgang til beskyttede ressurser. Programmerere gjør dette for å sikre at kun autoriserte brukere kan hente eller endre data.

## Hvordan:

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")

-- Opprett en base64-enkodet streng for autentisering
local function basic_auth(user, password)
    return "Basic " .. (user .. ":" .. password):gsub("\n", ""):gsub("\r", "")
end

-- Sett opp HTTP-forespørselen med grunnleggende autentisering
local user, password = "brukernavn", "passord"
local response_body = {}

http.request{
    url = "http://eksempelside.no/data",
    method = "GET",
    headers = {
        ["Authorization"] = basic_auth(user, password)
    },
    sink = ltn12.sink.table(response_body)
}

-- Skriver ut responsen fra serveren
print(table.concat(response_body))
```

Eksempelutdata:
```
{"status":"suksess","melding":"Du er autentisert!"}
```

## Dypdykk

Før SSL/TLS ble utbredt, var grunnleggende autentisering over HTTP vanlig for å beskytte ressurser. Til tross for svakhetene fortsatt brukt, spesielt der enkelhet er prioritert. 

Digest-autentisering er et alternativ som er litt sikrere, da det ikke sender passord i klartekst. Moderne alternativer inkluderer OAuth og API-nøkler, som både gir mer robust sikkerhet og fleksibilitet.

Grunnleggende autentisering fungerer ved å inkludere en `Authorization` header i HTTP-forespørselen. Verdien er ordet "Basic" fulgt av en base64-enkodet streng av brukernavn og passord. Lua krever et eksternt bibliotek, som `socket.http` i vårt eksempel, for å håndtere HTTP-forespørsler.

## Se Også

- LuaSec for HTTPS-støtte: https://github.com/brunoos/luasec/wiki
- HTTP-biblioteker for Lua: http://w3.impa.br/~diego/software/luasocket/http.html
- Lua Socket dokumentasjon: http://w3.impa.br/~diego/software/luasocket/
- Base64-koding i Lua: https://lua-users.org/wiki/BaseSixtyFour
