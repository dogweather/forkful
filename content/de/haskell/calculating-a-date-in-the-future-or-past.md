---
title:    "Haskell: Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datum in der Zukunft oder Vergangenheit kann nützlich sein, um beispielsweise Termine oder Fristen zu planen oder zu überprüfen.

## Wie geht das?

In Haskell kann dies einfach mit der Funktion `addDays` aus dem `Data.Time`-Modul durchgeführt werden. Hier ist ein Beispiel, das das Datum, das in 30 Tagen sein wird, berechnet:

```Haskell
import Data.Time

currentDate <- getCurrentTime
let futureDate = addDays 30 (utctDay currentDate)
print futureDate
```

Die Ausgabe für `futureDate` wäre `2022-02-15`, was in 30 Tagen dem heutigen Datum entspricht.

## Tiefentauchen

Die `addDays`-Funktion verwendet den Datentyp `Day`, der in `Data.Time.Calendar` definiert ist, um Datumsoperationen durchzuführen. Der Datentyp `Day` ist eine Instanz des Typklassen `Num`, was bedeutet, dass wir arithmetische Operationen wie `addDays` auf ihm anwenden können.

In diesem Beispiel haben wir `utctDay` verwendet, um das aktuelle Datum aus der aktuellen `UTCTime`-Uhrzeit abzurufen. Wir haben der `addDays`-Funktion die Anzahl der Tage übergeben, um das zukünftige Datum zu berechnen. Die Funktion gibt dann das zukünftige Datum als `Day`-Objekt zurück, das wir dann ausdrucken können.

Um ein Datum in der Vergangenheit zu berechnen, können wir einfach eine negative Anzahl von Tagen an `addDays` übergeben.

## Siehe auch

- Die offizielle Haskell-Dokumentation für `Data.Time` (https://hackage.haskell.org/package/time/docs/Data-Time.html)
- Ein Tutorial zu Datum und Zeit in Haskell (https://wiki.haskell.org/Date_and_time)
- Weitere Funktionen für die Arbeit mit Datum in Haskell (https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)