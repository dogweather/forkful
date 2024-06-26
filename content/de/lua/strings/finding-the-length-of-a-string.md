---
date: 2024-01-20 17:47:39.861783-07:00
description: "How to (Wie geht das) In Lua, weist der `#` Operator schnell die L\xE4\
  nge eines Strings zu. Dies ist effizient, da Lua intern die L\xE4nge von Strings\u2026"
lastmod: '2024-04-05T22:51:08.559488-06:00'
model: gpt-4-1106-preview
summary: "Dies ist effizient, da Lua intern die L\xE4nge von Strings speichert."
title: "Ermittlung der Zeichenkettenl\xE4nge"
weight: 7
---

## How to (Wie geht das)
```lua
-- Einfaches Beispiel
local text = "Hallo Welt"
print(#text)  -- Gibt die Länge aus: 10

-- Bei Strings mit Sonderzeichen
local gemischterText = "Füße"
print(#gemischterText)  -- Kann je nach Encoding variieren

-- String-Länge in einer Funktion nutzen
local function stringLaenge(s)
  return #s
end
print(stringLaenge("Programmieren ist cool"))  -- Ausgabe: 22
```

## Deep Dive (Tiefer eintauchen)
In Lua, weist der `#` Operator schnell die Länge eines Strings zu. Dies ist effizient, da Lua intern die Länge von Strings speichert. Historisch gesehen, war diese einfache Methode nicht immer verfügbar in anderen Sprachen, wo die Länge mittels Schleifen ermittelt werden musste. UTF-8 Zeichen können mehrere Bytes belegen, deshalb kann `#` in Lua 5.3 und neuer bei Unicode-Strings irreführend sein. Alternativen wie `utf8.len` bieten hier eine Lösung an. Im Kern sucht Lua einfach das Null-Byte, welches das String-Ende markiert, um die Länge zu bestimmen.

## See Also (Siehe auch)
- Lua 5.4 Referenzhandbuch: https://www.lua.org/manual/5.4/
- Einführung in `utf8.len`: https://www.lua.org/manual/5.4/manual.html#6.5
- String-Manipulationsbibliotheken für fortgeschrittene Nutzung: http://lua-users.org/wiki/StringLibraryTutorial
