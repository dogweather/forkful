---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:34.310039-07:00
description: "Hoe: Lua bevat geen native XML-parser, maar er zijn bibliotheken zoals\
  \ LuaXML en xml2lua die het werk gedaan krijgen. Hier is een snelle blik op het\u2026"
lastmod: '2024-03-13T22:44:50.961301-06:00'
model: gpt-4-0125-preview
summary: Lua bevat geen native XML-parser, maar er zijn bibliotheken zoals LuaXML
  en xml2lua die het werk gedaan krijgen.
title: Werken met XML
weight: 40
---

## Hoe:
Lua bevat geen native XML-parser, maar er zijn bibliotheken zoals LuaXML en xml2lua die het werk gedaan krijgen. Hier is een snelle blik op het parseren van XML met xml2lua:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programmeren in Lua</book></root>]])

print(handler.root.book._attr.id)  -- Uitvoer: 123
print(handler.root.book[1])        -- Uitvoer: Programmeren in Lua
```

Voor het schrijven van XML, hier is een mini-voorbeeld met LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programmeren in Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- Uitvoer: <root><book id="123">Programmeren in Lua</book></root>
```

## Diepere duik
XML, kort voor Extensible Markup Language, is sinds de jaren '90 een standaard in de representatie en uitwisseling van gegevens. Het geeft structuur aan gegevens en is zowel voor mensen leesbaar als door machines te verwerken.

Hoewel JSON en YAML nu de voorkeur hebben vanwege hun eenvoud, blijft XML veel voorkomen in veel ondernemings- en legacy-systemen. In Lua is native XML-verwerking niet ingebouwd omdat Lua is ontworpen om klein en uitbreidbaar te zijn via modules.

XML-bibliotheken voor Lua, zoals LuaXML, xml2lua en anderen, overbruggen deze kloof. LuaXML biedt een lichtgewicht XML-lezer en -schrijver, terwijl xml2lua een op gebeurtenissen gebaseerde aanpak gebruikt, vergelijkbaar met SAX-parsers. Deze bibliotheken zijn meestal geïmplementeerd in pure Lua voor draagbaarheid, terwijl sommige mogelijk op C vertrouwen voor prestaties.

Als het gaat om prestaties en geheugengebruik, zijn de XML-bibliotheken van Lua misschien niet zo snel als die in talen met native ondersteuning. Echter, voor de meeste use-cases in Lua, met name in game-ontwikkeling of scripting voor ingebedde systemen, doen deze bibliotheken het prima zonder het systeem te overbelasten.

## Zie ook
- LuaXML op GitHub: https://github.com/LuaDist/luaxml
- xml2lua op GitHub: https://github.com/manoelcampos/xml2lua
- Lua.org's lijst van bibliotheken: https://lua-users.org/wiki/LibrariesAndBindings
