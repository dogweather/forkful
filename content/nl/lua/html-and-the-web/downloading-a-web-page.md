---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:22.915498-07:00
description: "Hoe: Lua is standaard niet uitgerust voor webtaken, maar met de `socket`\
  \ bibliotheek en `http` module, is het een fluitje van een cent. Hier is een snel\u2026"
lastmod: '2024-03-13T22:44:50.936205-06:00'
model: gpt-4-0125-preview
summary: Lua is standaard niet uitgerust voor webtaken, maar met de `socket` bibliotheek
  en `http` module, is het een fluitje van een cent.
title: Een webpagina downloaden
weight: 42
---

## Hoe:
Lua is standaard niet uitgerust voor webtaken, maar met de `socket` bibliotheek en `http` module, is het een fluitje van een cent. Hier is een snel voorbeeld met LuaSocket:

```Lua
-- Vergeet niet LuaSocket te installeren: `luarocks install luasocket`
local http = require("socket.http")
local body, code = http.request("http://www.example.com")

if code == 200 then
    print(body)  -- Succes! Toont de webpagina-inhoud.
else
    print("Er is iets misgegaan :(", code)
end
```

Voorbeelduitvoer:
```
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
...
```

## Diepgaand
Voor LuaSocket was het downloaden van webinhoud in Lua omslachtiger. Alternatieven zoals het gebruik van `io.popen` om `curl` of `wget` te bellen, waren gebruikelijk.

LuaSocket bestaat sinds 2004 en maakt netwerkinteracties zoals HTTP-verzoeken in Lua eenvoudig. Het werkt door TCP/IP socket API-aanroepen in eenvoudig te gebruiken Lua-functies in te pakken. Voor HTTPS kan LuaSec erbovenop worden gebruikt.

De uitbreidbaarheid van Lua betekent dat je ook andere Lua-gebaseerde frameworks of modules kunt gebruiken, zoals OpenResty voor meer complexe webinteracties binnen een high-performance webserveromgeving.

Houd er rekening mee dat, als je bezig bent met flinke web scraping of complexe verwerking, Lua misschien niet je eerste keuze is; Python met bibliotheken zoals Requests en Beautiful Soup kan je beter van dienst zijn.

## Zie Ook
- LuaSocket documentatie: http://w3.impa.br/~diego/software/luasocket/
- LuaSec (voor HTTPS-ondersteuning): https://github.com/brunoos/luasec/wiki
- OpenResty voor meer geavanceerde webinteracties: https://openresty.org/en/
