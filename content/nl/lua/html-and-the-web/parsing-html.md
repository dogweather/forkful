---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:41.702944-07:00
description: "Hoe te: Lua is van nature niet zo web-savvy als Python of JavaScript,\
  \ maar met de `luasocket` en `luahtml` libraries, kan het zich een weg banen in\
  \ het\u2026"
lastmod: '2024-03-13T22:44:50.935256-06:00'
model: gpt-4-0125-preview
summary: Lua is van nature niet zo web-savvy als Python of JavaScript, maar met de
  `luasocket` en `luahtml` libraries, kan het zich een weg banen in het territorium
  van HTML-parsing.
title: HTML Parsen
weight: 43
---

## Hoe te:
Lua is van nature niet zo web-savvy als Python of JavaScript, maar met de `luasocket` en `luahtml` libraries, kan het zich een weg banen in het territorium van HTML-parsing. Laten we duiken in een basisvoorbeeld:

```Lua
local socket = require("socket.http")
local html = require("luahtml")

-- HTML ophalen van een URL
local body, code = socket.request("http://voorbeeld.com")

if code ~= 200 then
    print("Pagina laden mislukt")
    return
end

-- De HTML parsen
local parsed_html = html.parse(body)

-- Data extraheren uit een specifiek element, bijvoorbeeld een paragraaf
for _, p in ipairs(parsed_html:select("p")) do
    print(p:getcontent())
end
```

Dit zal de inhoud van alle paragraaftags (`<p>`) van de opgehaalde webpagina afdrukken.

## Diepe Duik
HTML-parsing in Lua is geen kwestie van alles-in-één. Je moet verschillende libraries aan elkaar rijgen, in tegenstelling tot in talen die met web-parsing in gedachten zijn ontworpen. Historisch gezien is Lua een hulpje geweest voor snelle, ingebedde scripting in apps, niet voor web scraping.

Alternatieven? Naast `luahtml`, zijn er ook `luascrape` en `luaxpath` voor verschillende parsingbehoeften. Er is geen objectief 'beste' keuze - elke optie komt met eigenaardigheden waar je je weg in moet vinden.

Duikend in de implementatie, maken Lua libraries over het algemeen gebruik van de C API voor prestatievoordelen. Tijdens het doorzoeken van HTML, zul je knopen en elementen jongleren, elk een kans om de vervelende details van webstructuren na te jagen.

## Zie Ook
- LuaSocket documentatie: http://w3.impa.br/~diego/software/luasocket/http.html
- luahtml op GitHub voor een diepe duik in parsingmethoden: https://github.com/o-lim/luahtml
- Lua Gebruikers Wiki voor gemeenschapspareltjes en probleemoplossing: http://lua-users.org/wiki/
