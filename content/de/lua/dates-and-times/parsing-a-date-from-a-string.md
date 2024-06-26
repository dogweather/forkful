---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:43.838234-07:00
description: "Wie: Lua bietet keine eingebaute Unterst\xFCtzung f\xFCr die Manipulation\
  \ von Datum und Zeit \xFCber die begrenzte Funktionalit\xE4t der Funktionen `os.date`\
  \ und\u2026"
lastmod: '2024-03-13T22:44:54.024420-06:00'
model: gpt-4-0125-preview
summary: "Lua bietet keine eingebaute Unterst\xFCtzung f\xFCr die Manipulation von\
  \ Datum und Zeit \xFCber die begrenzte Funktionalit\xE4t der Funktionen `os.date`\
  \ und `os.time` hinaus."
title: Einen Datum aus einem String analysieren
weight: 30
---

## Wie:
Lua bietet keine eingebaute Unterstützung für die Manipulation von Datum und Zeit über die begrenzte Funktionalität der Funktionen `os.date` und `os.time` hinaus. Diese können jedoch für einfaches Parsen genutzt werden, und für komplexere Anforderungen kann die `luadate` Bibliothek, eine externe Bibliothek, verwendet werden.

**Verwendung von `os.date` und `os.time`:**
```lua
-- Ein für Menschen lesbares Datum in einen Zeitstempel umwandeln und zurück
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local year, month, day, hour, minute, second = dateString:match(pattern)

local timestamp = os.time({
  year = year,
  month = month,
  day = day,
  hour = hour,
  min = minute,
  sec = second
})

-- Zeitstempel zurück in ein für Menschen lesbares Format umwandeln
local formattedDate = os.date("%Y-%m-%d %H:%M:%S", timestamp)
print(formattedDate)  -- Ausgabe: 2023-09-21 15:00:00
```

**Verwendung von `luadate` (externe Bibliothek):**
Um `luadate` zu nutzen, stellen Sie sicher, dass es über LuaRocks oder Ihren bevorzugten Paketmanager installiert ist. `luadate` fügt umfangreiche Möglichkeiten zur Parsung und Manipulation von Datum und Zeit hinzu.

```lua
local date = require('date')

-- Ein Datumsstring direkt parsen
local parsedDate = date.parse("2023-09-21 15:00:00")
print(parsedDate:fmt("%Y-%m-%d %H:%M:%S"))  -- Ausgabe: 2023-09-21 15:00:00

-- Dauer hinzufügen
local oneWeekLater = parsedDate:adddays(7)
print(oneWeekLater:fmt("%Y-%m-%d %H:%M:%S"))  -- Ausgabe: 2023-09-28 15:00:00
```

Die `luadate` Bibliothek bietet eine intuitivere und leistungsfähigere Möglichkeit, mit Daten zu arbeiten, einschließlich dem Parsen aus Strings, Formatierung und arithmetischen Operationen an Daten, was das Arbeiten mit zeitlichen Daten in Lua erheblich vereinfacht.
