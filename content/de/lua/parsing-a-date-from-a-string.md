---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:37:36.296231-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"

category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen von Daten aus Textstrings ermöglicht es, menschenlesbare Daten in eine strukturierte Form umzuwandeln, die von Programmen genutzt werden kann. Es ist nützlich für die Verarbeitung von Formulareingaben, das Lesen von Konfigurationsdateien und viele andere Aufgaben, bei denen Datumswerte wichtig sind.

## So geht’s:
```Lua
-- Einfaches Beispiel für das Parsen eines Datum-Strings
local datum_str = "28.03.2023"

-- Zerlegen des Strings in Tag, Monat und Jahr
local tag, monat, jahr = datum_str:match("(%d+)%.(%d+)%.(%d+)")

-- Umwandeln der String-Werte in Zahlen
tag = tonumber(tag)
monat = tonumber(monat)
jahr = tonumber(jahr)

-- Ausgabe der Ergebnisse
print(tag, monat, jahr) -- Gibt aus: 28 3 2023
```

## Tiefgang
Das Parsen von Daten war schon immer ein wichtiges Thema in der Programmierung, da es grundlegend für die Interaktion mit Benutzern und den Umgang mit externen Datenquellen ist. In Lua gibt es keine eingebaute Funktion für komplexe Datumsverarbeitung, wie in anderen Sprachen wie JavaScript oder Python. Daher sind reguläre Ausdrücke oder spezielle Parsing-Funktionen oft notwendig.

Alternativ könnte man Lua-Module wie `os.date` für einfache Konvertierungen oder externe Bibliotheken wie `LuaDate` für umfassendere Datumsmanipulationen einsetzen. Die `os.date`-Funktion beispielsweise kann Datumswerte in verschiedene Formate konvertieren, ist jedoch beim Einlesen von Daten begrenzt.

Die Implementierung von datumsspezifischen Parsing-Funktionen in Lua erfordert daher oft eine Kombination aus Regular Expressions (mittels `string.match`) und manueller Validierung bzw. Konvertierung der extrahierten Werte, um sicherzustellen, dass die Daten sinnvoll und korrekt sind.

## Siehe auch
- Lua-Handbuch zu Patterns: https://www.lua.org/manual/5.4/manual.html#6.4.1
- LuaDate, eine externe Bibliothek für Datums- und Zeitfunktionalität: https://github.com/Tieske/date
- LuaRocks-Modulverzeichnis für Datums- und Zeitbibliotheken: https://luarocks.org/modules?q=date
