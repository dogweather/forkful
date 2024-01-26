---
title:                "Vergleich von zwei Daten"
date:                  2024-01-20T17:33:39.091355-07:00
model:                 gpt-4-1106-preview
simple_title:         "Vergleich von zwei Daten"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Datumvergleich bedeutet, zwei Zeitpunkte hinsichtlich ihrer chronologischen Reihenfolge zu vergleichen. Programmierer nutzen dies, um Zeitabläufe zu steuern, Dauer zu berechnen oder Ereignisse zu terminieren.

## Anleitung:
```Lua
os = require("os")

-- Zwei Datum-Strings im ISO 8601 Format: Jahr-Monat-Tag
local datum1 = "2023-04-01"
local datum2 = "2023-04-15"

-- Umwandlung der Strings in Zeitstempel
local zeitstempel1 = os.time({year=datum1:sub(1,4), month=datum1:sub(6,7), day=datum1:sub(9,10)})
local zeitstempel2 = os.time({year=datum2:sub(1,4), month=datum2:sub(6,7), day=datum2:sub(9,10)})

-- Vergleich der Zeitstempel
if zeitstempel1 > zeitstempel2 then
    print(datum1 .. " ist nach " .. datum2)
elseif zeitstempel1 < zeitstempel2 then
    print(datum1 .. " ist vor " .. datum2)
else
    print(datum1 .. " ist gleich " .. datum2)
end
```
Beispielausgabe:
```
2023-04-01 ist vor 2023-04-15
```

## Vertiefung:
Früher, als Computer noch in Kinderjahren steckten, gab es unterschiedlichste Methoden, um Daten zu vergleichen – oft komplexe, eigene Implementierungen. Heute nutzen wir eingebaute Libs wie `os.time`, die die Hürden beseitigen. Alternativ könnten wir auch die `os.difftime` Funktion verwenden, um die Sekundendifferenz zwischen zwei Zeitstempeln zu erreichen. Intern behandelt Lua Datum und Zeit als Sekunden seit der Epoche (1. Januar 1970), was den Vergleich wesentlich vereinfacht.

## Siehe auch:
- Die Lua-Dokumentation für das `os`-Modul: [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.9)
- Ein Tutorial für fortgeschrittene Datum- und Zeitfunktionen in Lua: [Programming in Lua](https://www.lua.org/pil/22.1.html)
