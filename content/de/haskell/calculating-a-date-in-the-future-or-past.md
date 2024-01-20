---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Haskell: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

**## Was & Warum?**

Das Berechnen eines Datums in der Zukunft oder Vergangenheit sind Prozesse, die ein bestimmtes Datum nehmen und davon einen anderen Zeitpunkt ableiten. Programmierer machen das oft, um Termine oder Fristen zu planen und zu verwalten.

**## Wie es geht:**

Wir verwenden in Haskell die Bibliothek `Data.Time` für diese Aufgabe. Hier ist ein einfaches Code-Beispiel:

```Haskell
import Data.Time

addDaysToCurrentDate :: Integer -> IO Day
addDaysToCurrentDate n = do
    currentDate <- getCurrentTime
    let currentDay = utctDay currentDate
    return (addDays n currentDay)
```

Dieser Code ergibt ein Datum, das n Tage vom aktuellen Datum entfernt ist.

**## Deep Dive**

In der Vergangenheit gab es keine eingebaute Fähigkeit, um Daten in Haskell zu manipulieren. Das bedeutete, dass Entwickler pack-string Daten konvertieren und manipulieren mussten. Glücklicherweise haben wir jetzt die `Data.Time`-Bibliothek, die speziell für die Arbeit mit Zeiten in Haskell entwickelt wurde.

Einige Programmierer könnte die Bibliothek `time-lens` bevorzugen. Sie stellt Funktionen bereit, die die Arbeit mit Zeiten einfacher macht und bietet sogar die Möglichkeit, algebraische Datenstrukturen zu erstellen.

Die `addDays` Funktion nimmt unser aktuelles Datum (ein `Day`-Art) und eine Ganzzahl n. Sie gibt ein anderes `Day`-Art zurück, das n Tage entfernt ist. Beachten jedoch, dass `addDays` mit negativer Zahl auch ein Letztes Datum berechnen kann.

**## Siehe Auch**

Für weitere Funktionen und Details können Sie die offizielle Dokumentation zu `Data.Time` [hier](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html) und `time-lens` [hier](https://hackage.haskell.org/package/time-lens-0.4.0.2/docs/Data-Time-Lens.html) prüfen. Denken Sie daran, dass das Erlernen der Grundlagen der Datums- und Zeitmanipulation in Haskell Sie dabei unterstützen kann, komplexe Zeitanforderungen in Ihren eigenen Programmen zu lösen.