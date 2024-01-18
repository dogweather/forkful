---
title:                "Vergleich von zwei Daten"
html_title:           "Lua: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Vergleichung von zwei Daten ist eine häufige Aufgabe in der Programmierung. Dabei wird geprüft, ob ein bestimmtes Datum früher oder später als ein anderes ist. Programmierer verwenden dies oft, um zu überprüfen, ob ein Datum in der Zukunft oder Vergangenheit liegt oder um Dauern zwischen zwei Daten zu berechnen.

## Wie geht's?

```Lua
-- Vergleichen von zwei Daten
if date1 > date2 then
    print("date1 liegt später als date2")
elseif date1 < date2 then
    print("date1 liegt früher als date2")
else
    print("beide Daten sind gleich")
end

-- Berechnen von Dauern zwischen zwei Daten
days = (date2 - date1) / (60*60*24)
print("Die Dauer zwischen date1 und date2 ist:", days, "Tage")
```

### Beispiele:
- ```Lua
date1 = "2021-01-01"
date2 = "2022-01-01"
```
Ausgabe: date1 liegt früher als date2
Die Dauer zwischen date1 und date2 ist: 365 Tage

- ```Lua
date1 = "2020-02-01"
date2 = "2020-01-01"
```
Ausgabe: date1 liegt später als date2
Die Dauer zwischen date1 und date2 ist: -31 Tage

- ```Lua
date1 = "2020-12-31"
date2 = "2020-12-31"
```
Ausgabe: beide Daten sind gleich
Die Dauer zwischen date1 und date2 ist: 0 Tage

## Tiefgehende Informationen

Die Vergleichung von Daten ist eng mit der Chronologie und Zeitrechnung verbunden. In der Geschichte hat es verschiedene Kalendersysteme gegeben, um die Zeit zu messen. Heutzutage werden meist der Gregorianische oder der Julianische Kalender verwendet. Beide haben ihre eigenen Regeln für das Fortschreiten der Zeit und die Berechnung von Schaltjahren, was zu Unterschieden bei der Vergleichung von Daten führen kann.

Es gibt auch andere Methoden, um Dauern zwischen Daten zu berechnen, wie zum Beispiel die Verwendung von Zeitstempeln, die die Anzahl der Sekunden seit dem 1. Januar 1970 zählen.

Bei der Implementierung der Vergleichung von Daten muss beachtet werden, dass diese für verschiedene Datenformate und internationale Zeichenformate funktionieren sollte.

## Siehe auch

- [Lua-Referenzhandbuch](https://www.lua.org/manual/5.4/)
- [Vergleichung von Daten in JavaScript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Alternativen zur Berechnung von Dauern zwischen Daten](https://www.freecodecamp.org/news/alternatives-to-dating-like-data-comparison-in-javascript-6ae0cd839c5c/)