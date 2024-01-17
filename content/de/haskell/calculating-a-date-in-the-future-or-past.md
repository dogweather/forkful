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

## Was ist das und warum?

Das Berechnen eines zukünftigen oder vergangenen Datums ist ein wichtiger Teil der Programmierung, da es uns ermöglicht, dynamische und zeitabhängige Funktionen in unsere Programme einzubauen. Zum Beispiel können wir mit einer solchen Funktion eine Erinnerung für einen bestimmten Termin erstellen oder eine Zeitsteuerung für bestimmte Aktionen einrichten.

## Wie geht das?

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die Standardbibliothek von Haskell verwenden, die die Funktion `addDays` bereitstellt. Diese Funktion nimmt ein Datum und eine Anzahl von Tagen an und gibt das entsprechende Datum in der Zukunft oder Vergangenheit zurück.

```Haskell
import Data.Time.Calendar ( fromGregorian, addDays )

let date = fromGregorian 2020 7 15 -- 15. Juli 2020
let futureDate = addDays 10 date -- 25. Juli 2020
let pastDate = addDays (-5) date -- 10. Juli 2020

print futureDate -- 2020-07-25
print pastDate -- 2020-07-10
```

In diesem Beispiel verwenden wir das `Data.Time.Calendar` Modul, um ein Datum in Haskell zu erstellen. Wir weisen dem Datum den 15. Juli 2020 zu und nutzen dann `addDays`, um 10 Tage in die Zukunft und 5 Tage in die Vergangenheit zu berechnen. 

## Tiefere Informationen

Die Möglichkeit, ein Datum in Zukunft oder Vergangenheit zu berechnen, hat ihre Wurzeln in der Julianischen Tageszählung, die im alten Rom verwendet wurde. Heutzutage gibt es auch andere Bibliotheken oder externe APIs, die es ermöglichen, Datumsberechnungen durchzuführen, wie zum Beispiel das beliebte `time` Paket.

Bei der Berechnung von Datumsangaben müssen wir auch berücksichtigen, dass es in verschiedenen Ländern unterschiedliche Kalender und Datumsformate gibt. Daher ist es wichtig, dass wir uns bewusst sind, welche Kalendernotation wir in unserem Programm verwenden.

## Siehe auch

- [Das `time` Paket für die Berechnung von Datumsangaben in Haskell](https://hackage.haskell.org/package/time)