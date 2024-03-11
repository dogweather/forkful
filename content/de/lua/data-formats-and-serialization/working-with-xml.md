---
date: 2024-01-26 04:33:24.900880-07:00
description: "Die Arbeit mit XML umfasst das Parsen und Manipulieren von XML-Dokumenten\
  \ mithilfe von Code. Programmierer machen dies, um Daten in einem strukturierten,\u2026"
lastmod: '2024-03-11T00:14:27.938252-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit XML umfasst das Parsen und Manipulieren von XML-Dokumenten\
  \ mithilfe von Code. Programmierer machen dies, um Daten in einem strukturierten,\u2026"
title: Arbeiten mit XML
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit XML umfasst das Parsen und Manipulieren von XML-Dokumenten mithilfe von Code. Programmierer machen dies, um Daten in einem strukturierten, portablen Format zu lesen, zu schreiben und zu modifizieren, das weit verbreitet für Datenaustausch und Speicherung verwendet wird.

## Wie geht das:
Lua enthält kein natives XML-Parsing, aber es gibt Bibliotheken wie LuaXML und xml2lua, die diese Aufgabe erledigen. Hier ein kurzer Blick darauf, wie man XML mit xml2lua parst:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programmierung in Lua</book></root>]])

print(handler.root.book._attr.id)  -- Ausgabe: 123
print(handler.root.book[1])        -- Ausgabe: Programmierung in Lua
```

Für das Schreiben von XML, hier ein Mini-Beispiel mit LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programmierung in Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- Ausgabe: <root><book id="123">Programmierung in Lua</book></root>
```

## Tiefere Einblicke
XML, kurz für Extensible Markup Language, ist seit Mitte der 90er Jahre ein Standard in der Datenrepräsentation und im Datenaustausch. Es gibt Daten eine Struktur und ist sowohl für Menschen lesbar als auch für Maschinen verarbeitbar.

Während JSON und YAML heute wegen ihrer Einfachheit bevorzugt werden, bleibt XML in vielen Unternehmens- und Legacy-Systemen verbreitet. In Lua ist die native XML-Verarbeitung nicht eingebaut, weil Lua darauf ausgelegt ist, klein und durch Module erweiterbar zu sein.

XML-Bibliotheken für Lua, wie LuaXML, xml2lua und andere, schließen diese Lücke. LuaXML bietet einen leichten XML-Leser und -Schreiber, während xml2lua einen ereignisgesteuerten Ansatz verwendet, der SAX-Parsern ähnelt. Diese Bibliotheken sind meist in reinem Lua für Portabilität implementiert, während einige möglicherweise auf C für Leistung setzen.

Wenn es um Leistung und Speicherverbrauch geht, sind die XML-Bibliotheken von Lua möglicherweise nicht so schnell wie die in Sprachen mit nativer Unterstützung. Jedoch, für die meisten Anwendungsfälle in Lua, insbesondere in der Spieleentwicklung oder beim Scripting für eingebettete Systeme, machen diese Bibliotheken einen guten Job, ohne das System zu überlasten.

## Siehe auch
- LuaXML auf GitHub: https://github.com/LuaDist/luaxml
- xml2lua auf GitHub: https://github.com/manoelcampos/xml2lua
- Lua.org's Liste von Bibliotheken: https://lua-users.org/wiki/LibrariesAndBindings
