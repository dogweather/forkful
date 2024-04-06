---
date: 2024-01-20 17:37:04.916331-07:00
description: "So geht's: Das os.date-Modul in Lua bietet Funktionen, um Datums- und\
  \ Zeitinformationen zu manipulieren. Seit Lua 5.1 gibt es diese Funktionen, und\
  \ sie\u2026"
lastmod: '2024-04-05T21:53:55.909580-06:00'
model: gpt-4-1106-preview
summary: Das os.date-Modul in Lua bietet Funktionen, um Datums- und Zeitinformationen
  zu manipulieren.
title: Datum in einen String umwandeln
weight: 28
---

## So geht's:
```Lua
-- Aktuelles Datum und Uhrzeit holen
local aktuellesDatum = os.date("*t")

-- Datum in einen String umwandeln
local datumString = string.format("Heute ist der %02d.%02d.%04d", aktuellesDatum.day, aktuellesDatum.month, aktuellesDatum.year)

print(datumString)  -- z.B. "Heute ist der 05.04.2023"
```

## Deep Dive
Das os.date-Modul in Lua bietet Funktionen, um Datums- und Zeitinformationen zu manipulieren. Seit Lua 5.1 gibt es diese Funktionen, und sie ähneln der C Standardbibliothek. Für Alternativen: strftime und andere externe Bibliotheken könnten verwendet werden, aber os.date ist die Standardlösung in Lua. Beachte: die Verwendung eines falschen Formats kann zu Fehlern oder falschen Ausgaben führen.

## Siehe Auch:
- Lua-Handbuch zur `os.date` Funktion: https://www.lua.org/manual/5.4/manual.html#6.9
- Lua-Community Diskussionen über Datum und Zeit: https://www.lua.org/wshop17/Alois.pdf
- Ein tieferer Einblick in Lua's os-Bibliothek: https://www.lua.org/pil/22.1.html
