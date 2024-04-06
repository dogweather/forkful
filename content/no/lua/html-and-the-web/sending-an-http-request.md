---
date: 2024-01-20 18:00:17.712777-07:00
description: "Hvordan: Eksempel p\xE5 output."
lastmod: '2024-04-05T21:53:41.891477-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\xC5 sende en HTTP-foresp\xF8rsel"
weight: 44
---

## Hvordan:
```Lua
-- Last inn HTTP-biblioteket
local http = require("socket.http")

-- URL til webtjenesten
local url = "http://www.example.com/api"

-- Send en GET-forespørsel
local respons, statuskode, headere = http.request(url)

-- Sjekk statuskode og skriv ut responsen
if statuskode == 200 then
    print("Suksess:", respons)
else
    print("Feil:", statuskode)
end
```
Eksempel på output:
```
Suksess: {"message": "Hei, verden!"}
```

## Dykk Ned:
Tilbake i 1991 kom HTTP, designet av Tim Berners-Lee. Det ble raskt internettets ryggrad. Lua støtter ikke HTTP rett ut av boksen. Du må bruke et bibliotek, som `socket.http` som følger med LuaSocket. Alternativer inkluderer luasec, for HTTPS-støtte (som er HTTP over SSL/TLS). Å sende en HTTP-forespørsel i Lua krever å håndtere URLer, HEADere, og kanskje til og med datakoding. Pass på dataformatet (som JSON) når du sender eller mottar mer komplekse data.

## Se Også:
- LuaSocket dokumentasjon: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec for HTTPS-støtte: https://github.com/brunoos/luasec/wiki
- Lua-users wiki om nettverksprogrammering: http://lua-users.org/wiki/NetworkingAndInternet
- JSON i Lua med 'dkjson': http://dkolf.de/src/dkjson-lua.fsl/home
