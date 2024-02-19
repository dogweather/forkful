---
aliases:
- /nl/lua/downloading-a-web-page/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:22.915498-07:00
description: "Een webpagina downloaden betekent het grijpen van de HTML-inhoud van\
  \ het internet via de URL. Programmeurs doen dit om webinhoud te analyseren, taken\
  \ te\u2026"
lastmod: 2024-02-18 23:09:01.989945
model: gpt-4-0125-preview
summary: "Een webpagina downloaden betekent het grijpen van de HTML-inhoud van het\
  \ internet via de URL. Programmeurs doen dit om webinhoud te analyseren, taken te\u2026"
title: Een webpagina downloaden
---

{{< edit_this_page >}}

## Wat & Waarom?
Een webpagina downloaden betekent het grijpen van de HTML-inhoud van het internet via de URL. Programmeurs doen dit om webinhoud te analyseren, taken te automatiseren of gegevens in hun apps te integreren.

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
