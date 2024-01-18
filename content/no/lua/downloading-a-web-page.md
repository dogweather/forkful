---
title:                "Nedlasting av en nettside"
html_title:           "Lua: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å laste ned en nettside er å hente innholdet fra en nettside og lagre det lokalt på datamaskinen din. Dette gjøres vanligvis av programmører for å få tilgang til og manipulere data fra nettsider, som for eksempel å hente informasjon fra en nettbutikk eller et nettsted.

## Slik gjør du:
Det er mange måter å laste ned en nettside på i Lua, men en enkel måte er å bruke "socket" biblioteket. Her er en kodebit som viser dette:

```lua
local http = require("socket.http")
local body, status, headers = http.request("https://www.nettside.no")
```

Her bruker vi "socket.http" biblioteket for å hente nettsiden til variabelen "body". Vi kan deretter bruke denne variabelen til å få tilgang til innholdet på nettsiden. Status og headers variablene vil inneholde informasjon om respons fra nettsiden, som for eksempel om siden ikke er tilgjengelig.

## Dykk dypere:
Historisk sett har Lua ikke hatt innebygget støtte for å laste ned nettsider, men dette har blitt løst ved å bruke ulike eksterne biblioteker. Et populært alternativ til "socket" er "LuaCURL" som gir mer avanserte funksjoner for å laste ned nettsider. Lua har også nylig fått støtte for asynkron nettverkskall gjennom "cohttp" biblioteket.

## Se også:
- [LuaCURL](https://github.com/Lua-cURL/Lua-cURLv3)
- [cohttp](https://github.com/diegonehab/luasocket/blob/master/doc/protocol/http.md)