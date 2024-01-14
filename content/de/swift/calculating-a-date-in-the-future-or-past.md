---
title:                "Swift: Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich mit der Berechnung eines Datums in der Zukunft oder Vergangenheit beschäftigen? Ganz einfach: Es kann sehr nützlich sein, um zeitbezogene Aufgaben in Programmen zu automatisieren.

## Wie
Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, gibt es verschiedene Methoden in Swift. Eine Möglichkeit ist die Verwendung der Methode `dateByAdding` in der Klasse `NSDate`. Hier ein Beispiel, um 30 Tage zu einem gegebenen Datum hinzu zu addieren:

```Swift
let calendar = NSCalendar.currentCalendar()
let currentDate = NSDate()
let futureDate = calendar.dateByAdding(.Day, value: 30, toDate: currentDate, options: [])
print(futureDate)
```
Dies wird das Datum 30 Tage in der Zukunft ausgeben.

Um ein Datum in der Vergangenheit zu berechnen, können Sie einen negativen Wert verwenden. Zum Beispiel um 2 Monate und 5 Tage von einem gegebenen Datum abzuziehen:

```Swift
let calendar = NSCalendar.currentCalendar()
let currentDate = NSDate()
let pastDate = calendar.dateByAdding(.Month, value: -2, toDate: currentDate, options: [])
pastDate = calendar.dateByAdding(.Day, value: -5, toDate: pastDate, options: [])
print(pastDate)
```
Dies wird das Datum 2 Monate und 5 Tage in der Vergangenheit ausgeben.

## Deep Dive
Wenn Sie tiefer in die Welt der Datumsberechnungen einsteigen möchten, können Sie sich mit der Klasse `NSCalendar` vertraut machen. Diese bietet viele nützliche Methoden wie zum Beispiel `components:fromDate:` um einzelne Komponenten wie Tag, Monat oder Jahr von einem Datum zu erhalten.

## Siehe auch
- [NSCalendar Dokumentation](https://developer.apple.com/documentation/foundation/nscalendar)
- [NSDate Dokumentation](https://developer.apple.com/documentation/foundation/nsdate)
- [Swift Date Calculations Tutorial](https://www.raywenderlich.com/74890/swift-date-calculation-nscalendar-nsdatecomponents)