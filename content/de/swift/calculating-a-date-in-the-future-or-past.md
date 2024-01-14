---
title:    "Swift: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Daten ist ein häufiger Bestandteil der Programmierung. Egal ob es darum geht, einen Terminplaner oder eine Erinnerungsfunktion zu erstellen, die korrekte Berechnung von Datumsangaben ist von entscheidender Bedeutung.

## Wie geht's

Um ein zukünftiges Datum zu berechnen, können wir die Methode `date(byAdding:to:)` aus der `Calendar` Klasse nutzen. Diese Methode nimmt als ersten Parameter eine `DateComponents` Instanz und als zweiten Parameter ein Datum, zu dem die Komponenten hinzugefügt werden sollen. Zum Beispiel:

```Swift
let calendar = Calendar.current
let date = Date()

// Berechne ein Datum, das 5 Tage in der Zukunft liegt
var fiveDaysFromNow = calendar.date(byAdding: .day, value: 5, to: date)

// Berechne ein Datum, das 2 Monate in der Vergangenheit liegt
var twoMonthsAgo = calendar.date(byAdding: .month, value: -2, to: date)
```

Das Ergebnis dieser Berechnungen ist eine `Date` Instanz, die wir dann weiterverarbeiten können.

## Tief Einblick

Es gibt verschiedene Möglichkeiten, Daten in der Zukunft oder Vergangenheit zu berechnen. Neben der oben gezeigten Methode gibt es auch die Möglichkeit, den `Calendar` direkt zu manipulieren und damit zukünftige oder vergangene Daten zu erstellen. Eine weitere interessante Funktion ist auch `dateComponents(_:from:)` aus der `Calendar` Klasse, mit der wir eine Datumsangabe in ihre einzelnen Komponenten zerlegen können.

## Siehe auch

- [Apple Dokumentation zur `Calendar` Klasse](https://developer.apple.com/documentation/foundation/calendar)
- [Tutorial zur Arbeit mit Datum und Zeit in Swift](https://www.raywenderlich.com/767-swift-date-and-time-tutorial-for-ios)
- [Video über die Verwendung von DateComponents in Swift](https://www.youtube.com/watch?v=_ilpNYwgL-E)