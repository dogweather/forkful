---
aliases:
- /sv/lua/working-with-xml/
date: 2024-01-26 04:33:30.810392-07:00
description: "Att arbeta med XML inneb\xE4r att tolka och manipulera XML-dokument\
  \ med hj\xE4lp av kod. Programmerare g\xF6r detta f\xF6r att l\xE4sa, skriva och\
  \ \xE4ndra data i ett\u2026"
lastmod: 2024-02-18 23:08:51.942133
model: gpt-4-0125-preview
summary: "Att arbeta med XML inneb\xE4r att tolka och manipulera XML-dokument med\
  \ hj\xE4lp av kod. Programmerare g\xF6r detta f\xF6r att l\xE4sa, skriva och \xE4\
  ndra data i ett\u2026"
title: Att arbeta med XML
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML innebär att tolka och manipulera XML-dokument med hjälp av kod. Programmerare gör detta för att läsa, skriva och ändra data i ett strukturerat, portabelt format som är brett använt för datautbyte och lagring.

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
