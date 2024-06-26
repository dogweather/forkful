---
date: 2024-01-26 04:33:30.810392-07:00
description: "Hur man g\xF6r: Lua inkluderar inte inbyggd XML-tolkning, men det finns\
  \ bibliotek som LuaXML och xml2lua som g\xF6r jobbet. H\xE4r \xE4r en snabb titt\
  \ p\xE5 att tolka\u2026"
lastmod: '2024-03-13T22:44:38.063102-06:00'
model: gpt-4-0125-preview
summary: "Lua inkluderar inte inbyggd XML-tolkning, men det finns bibliotek som LuaXML\
  \ och xml2lua som g\xF6r jobbet."
title: Att arbeta med XML
weight: 40
---

## Hur man gör:
Lua inkluderar inte inbyggd XML-tolkning, men det finns bibliotek som LuaXML och xml2lua som gör jobbet. Här är en snabb titt på att tolka XML med xml2lua:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programming in Lua</book></root>]])

print(handler.root.book._attr.id)  -- Skriver ut: 123
print(handler.root.book[1])        -- Skriver ut: Programming in Lua
```

För att skriva XML, här är ett miniexempel som använder LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programming in Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- Skriver ut: <root><book id="123">Programming in Lua</book></root>
```

## Djupdykning
XML, som står för Extensible Markup Language, har varit en standard i datarrepresentation och utbyte sedan mitten av 90-talet. Det ger struktur till data och är både läsbart för människor och tolkbart för maskiner.

Även om JSON och YAML nu föredras för deras enkelhet, är XML fortfarande utbrett i många företag och äldre system. I Lua är inbyggd hantering av XML inte inbyggd eftersom Lua är designat för att vara litet och utbyggbart genom moduler.

XML-bibliotek för Lua, som LuaXML, xml2lua och andra, överbryggar detta gap. LuaXML tillhandahåller en lättvikts XML-läsare och -skrivare, medan xml2lua använder en händelsedriven metod liknande SAX-tolkar. Dessa bibliotek är vanligtvis implementerade i ren Lua för portabilitet, medan vissa kan förlita sig på C för prestanda.

När det kommer till prestanda och minnesanvändning kanske Luas XML-bibliotek inte är lika snabba som de i språk med inbyggt stöd. Men för de flesta användningsfall i Lua, speciellt inom spelutveckling eller skriptning för inbäddade system, gör dessa bibliotek ett bra jobb utan att överbelasta systemet.

## Se även
- LuaXML på GitHub: https://github.com/LuaDist/luaxml
- xml2lua på GitHub: https://github.com/manoelcampos/xml2lua
- Lua.org:s lista över bibliotek: https://lua-users.org/wiki/LibrariesAndBindings
