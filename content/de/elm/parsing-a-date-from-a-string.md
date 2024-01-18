---
title:                "Ein Datum aus einem String analysieren"
html_title:           "Elm: Ein Datum aus einem String analysieren"
simple_title:         "Ein Datum aus einem String analysieren"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String bedeutet, dass man einen Text in ein von der Programmiersprache verständliches Dateiformat umwandelt. Programmierer machen dies, um z.B. Benutzereingaben zu verarbeiten oder Datenbankabfragen zu erstellen.

## Wie geht's:
Um ein Datum aus einem String zu parsen, können wir die `Date.fromString` Funktion in Elm verwenden. 
```Elm 
Date.fromString "10/14/2020"
```
Dies gibt uns ein `Maybe Date` zurück, da der String möglicherweise kein gültiges Datum enthält. 
So können wir mit `Maybe.withDefault` eine Standardwert festlegen, falls kein gültiges Datum zurückgegeben wird. 
```Elm
Maybe.withDefault (Date.fromCalendarDate 2020 10 14) (Date.fromString "30/02/2020") 
--> { year = 2020, month = 10, day = 14 }
```

## Tiefentauchen:
Die Notwendigkeit, Daten aus einem String zu parsen, entstand mit der zunehmenden Nutzung von digitalen Geräten und der Notwendigkeit, unterschiedliche Datumseingaben standardisiert zu verarbeiten. 
Alternativen wie die Verwendung von Regular Expressions können unzuverlässig und fehleranfällig sein. 
Die `Date.fromString` Funktion nutzt die von der ECMAScript-Sprache definierten Regeln für die Interpretation von Datumsangaben.

## Siehe Auch:
[Die Elm Dokumentation zur Date.fromString Funktion] (https://package.elm-lang.org/packages/elm/time/latest/Date#fromString) 
[Der Elm Zeit-package] (https://package.elm-lang.org/packages/elm/time/latest/) für weitere zeitbezogene Funktionen.