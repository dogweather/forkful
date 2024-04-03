---
date: 2024-01-26 04:33:58.855864-07:00
description: "\xC5 jobbe med XML inneb\xE6rer parsing og manipulering av XML-dokumenter\
  \ ved hjelp av kode. Programmerere gj\xF8r dette for \xE5 lese, skrive og modifisere\
  \ data i et\u2026"
lastmod: '2024-03-13T22:44:40.954815-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med XML inneb\xE6rer parsing og manipulering av XML-dokumenter\
  \ ved hjelp av kode."
title: "\xC5 jobbe med XML"
weight: 40
---

## Hvordan:
Lua inkluderer ikke innebygd XML-parsing, men det finnes biblioteker som LuaXML og xml2lua som får jobben gjort. Her er et kjapt blikk på parsing av XML med xml2lua:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programming in Lua</book></root>]])

print(handler.root.book._attr.id)  -- Utgår: 123
print(handler.root.book[1])        -- Utgår: Programming in Lua
```

For å skrive XML, her er et mini-eksempel som bruker LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programming in Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- Utgår: <root><book id="123">Programming in Lua</book></root>
```

## Dypdykk
XML, kort for Extensible Markup Language, har vært en standard i datarepresentasjon og utveksling siden midten av 90-tallet. Det gir struktur til data og er både lesbar for mennesker og maskinparsbar.

Selv om JSON og YAML nå er favorisert for sin enkelhet, forblir XML utbredt i mange bedrifts- og arvesystemer. I Lua er ikke innfødt XML-håndtering innebygd fordi Lua er designet for å være liten og utvidbar gjennom moduler.

XML-biblioteker for Lua, som LuaXML, xml2lua, og andre, brobygger dette gapet. LuaXML tilbyr en lettvekts XML-leser og -skriver, mens xml2lua bruker en hendelsesdrevet tilnærming som ligner SAX-parsere. Disse bibliotekene er vanligvis implementert i ren Lua for portabilitet, mens noen kan være avhengige av C for ytelse.

Når det kommer til ytelse og minnebruk, kan Lua's XML-biblioteker ikke være like raske som de i språk med innebygd støtte. Imidlertid, for de fleste bruksområder i Lua, spesielt i spillutvikling eller skripting for innebygde systemer, gjør disse bibliotekene en fin jobb uten å overbelaste systemet.

## Se Også
- LuaXML på GitHub: https://github.com/LuaDist/luaxml
- xml2lua på GitHub: https://github.com/manoelcampos/xml2lua
- Lua.orgs liste over biblioteker: https://lua-users.org/wiki/LibrariesAndBindings
