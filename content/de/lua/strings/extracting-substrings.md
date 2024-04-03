---
date: 2024-01-20 17:46:13.613912-07:00
description: 'So geht''s: .'
lastmod: '2024-03-13T22:44:54.004303-06:00'
model: gpt-4-1106-preview
summary: .
title: Teilstrings extrahieren
weight: 6
---

## So geht's:
```Lua
-- Einen String definieren
local gesamttext = "Hallo Welt, das ist Lua!"

-- Teilstring extrahieren mit string.sub
local begruessung = string.sub(gesamttext, 1, 5) -- "Hallo"
local ort = string.sub(gesamttext, 7, 10)        -- "Welt"

print(begruessung) -- Gibt "Hallo" aus
print(ort)         -- Gibt "Welt" aus

-- Negative Indizes nutzen, um von hinten zu zählen
local sprache = string.sub(gesamttext, -3)       -- "Lua"
print(sprache)     -- Gibt "Lua" aus
```

## Deep Dive
Das Extrahieren von Teilstrings gibt es in fast jeder Programmiersprache. In Lua ist `string.sub` eine gängige Funktion dafür und wurde von anderen Sprachen wie C inspiriert. Alternativen in Lua könnten Musterabgleich (pattern matching) mit `string.match` oder die Verwendung von `string.gmatch` für einen Iterator über Treffer sein. Bei `string.sub` sind die Indizes 1-basiert, was im Gegensatz zu vielen anderen Programmiersprachen steht, die bei 0 anfangen.

## Siehe auch:
- Lua-Handbuch zu Strings: https://www.lua.org/manual/5.4/manual.html#6.4
- Online-Lua-Interpreter zum Experimentieren: https://repl.it/languages/lua 
- Lua String Manipulation Tutorial: https://www.tutorialspoint.com/lua/lua_strings.htm
