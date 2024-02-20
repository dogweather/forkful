---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:06.697703-07:00
description: "Das Abrufen des aktuellen Datums in der Programmierung ist eine entscheidende\
  \ Aufgabe f\xFCr eine Vielzahl von Anwendungen, einschlie\xDFlich Logging,\u2026"
lastmod: 2024-02-19 22:05:12.956434
model: gpt-4-0125-preview
summary: "Das Abrufen des aktuellen Datums in der Programmierung ist eine entscheidende\
  \ Aufgabe f\xFCr eine Vielzahl von Anwendungen, einschlie\xDFlich Logging,\u2026"
title: Den aktuellen Datum abrufen
---

{{< edit_this_page >}}

## Was & Warum?

Das Abrufen des aktuellen Datums in der Programmierung ist eine entscheidende Aufgabe für eine Vielzahl von Anwendungen, einschließlich Logging, Zeitstempelung von Ereignissen oder Planen von Aufgaben. In Lua ermöglicht diese Funktionalität es Programmierern, Datums- und Zeitoperationen nahtlos in ihre Anwendungen zu integrieren, um sicherzustellen, dass ihre Software effektiv mit Echtzeitdaten interagieren kann.

## Wie:

Lua stellt die Funktion `os.date` bereit, um das aktuelle Datum und die Uhrzeit zu erhalten. Die Funktion kann ohne Argumente verwendet werden, um eine formatierte Zeichenkette zu erhalten, oder mit Formatierungsspezifikatoren, um die Ausgabe anzupassen. So verwenden Sie es:

```lua
-- Das aktuelle Datum und die Uhrzeit als formatierte Zeichenkette erhalten
print(os.date())  -- z.B. Thu Mar  3 14:02:03 2022

-- Anpassen des Ausgabeformats
-- %Y für Jahr, %m für Monat, %d für Tag, %H für Stunde, %M für Minuten
print(os.date("%Y-%m-%d %H:%M"))  -- z.B. 2022-03-03 14:02
```

Für ausgefeiltere Datums- und Zeitmanipulationen hat Lua keine so umfangreichen eingebauten Bibliotheken wie einige andere Programmiersprachen. Sie können jedoch Drittanbieterbibliotheken wie `lua-date` (https://github.com/Tieske/date) verwenden. Diese Bibliothek bietet umfassendere Funktionen zur Manipulation von Daten und Zeiten. So könnten Sie sie verwenden:

Zuerst stellen Sie sicher, dass Sie die `lua-date` Bibliothek installiert haben. Typischerweise können Sie sie mit dem folgenden Befehl über LuaRocks installieren:

```bash
luarocks install lua-date
```

Dann können Sie sie in Ihrem Lua-Skript wie folgt verwenden:

```lua
local date = require("date")

-- Ein Date-Objekt für das aktuelle Datum und die Uhrzeit erstellen
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- z.B. 2022-03-03 14:02:03
```

Dieses Beispiel demonstriert die Erstellung eines `date`-Objekts, das den aktuellen Moment repräsentiert, den Sie dann ähnlich wie die Funktion `os.date` formatieren können, jedoch mit zusätzlicher Flexibilität und Optionen, die durch die `lua-date` Bibliothek bereitgestellt werden.
