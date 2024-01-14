---
title:                "Swift: Zwei Daten vergleichen"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Datumsangaben ist eine wichtige Aufgabe in der Swift Programmierung. Es ermöglicht dir, zu überprüfen, ob ein bestimmtes Datum vor oder nach einem anderen Datum liegt, was in vielen Anwendungen nützlich sein kann. In diesem Blogpost werden wir uns mit verschiedenen Techniken beschäftigen, um das Vergleichen von zwei Daten in Swift zu erlernen.

## Anleitung

Um zwei Datumsangaben in Swift zu vergleichen, gibt es zwei grundlegende Ansätze: die Verwendung des `compare` Operators oder die Verwendung der `Calendar` Klasse. Schauen wir uns zunächst den `compare` Operator an:

```Swift 
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600) // 1 Stunde später als date1

if date1 < date2 { // date1 ist vor date2
    print("Das Datum1 liegt vor Datum2")
} else if date1 > date2 { // date1 ist nach date2
    print("Das Datum1 liegt nach Datum2")
} else { // date1 und date2 sind gleich
    print("Das Datum1 ist gleich Datum2")
}
```

Die Ausgabe wird "Das Datum1 liegt vor Datum2" sein, da `date1` eine Stunde früher als `date2` ist. Beachte, dass wir den `compare` Operator (`<`, `>`) verwendet haben, um die Beziehung zwischen den beiden Datumsangaben zu bestimmen.

Alternativ können wir auch die `Calendar` Klasse verwenden:

```Swift 
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600) // 1 Stunde später als date1

let calendar = Calendar.current
let comparison = calendar.compare(date1, to: date2, toGranularity: .hour)

if comparison == .orderedAscending { // date1 ist vor date2
    print("Das Datum1 liegt vor Datum2")
} else if comparison == .orderedDescending { // date1 ist nach date2
    print("Das Datum1 liegt nach Datum2")
} else { // date1 und date2 sind gleich
    print("Das Datum1 ist gleich Datum2")
}
```

Auch hier wird die Ausgabe "Das Datum1 liegt vor Datum2" sein. Wir haben die `compare` Funktion der `Calendar` Klasse verwendet und die Granularität auf "hour" gesetzt, was bedeutet, dass wir nur das Datum bis zur Stunde vergleichen.

## Tiefer Eintauchen

Es gibt viele Optionen und Möglichkeiten, wenn es um das Vergleichen von Datumsangaben in Swift geht. Zum Beispiel können wir auch die `TimeInterval` Klasse verwenden, um die Differenz zwischen zwei Datumsangaben in Sekunden zu berechnen:

```Swift 
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600) // 1 Stunde später als date1

let interval = date2.timeIntervalSince(date1)
print("Die Differenz beträgt \(interval) Sekunden")
```

Erforsche und experimentiere mit verschiedenen Methoden, um das Vergleichen von Datumsangaben in Swift zu meistern.

## Siehe auch

- [Apple Dokumentation zum Vergleichen von Datumsangaben in Swift](https://developer.apple.com/documentation/foundation/date)
- [Swift Datumstypen und -operatoren](https://swift.org/blog/swift-3-0-released/)