---
title:                "Swift: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung ist das Nutzen des aktuellen Datums ein wichtiger Bestandteil für viele Anwendungen. Egal ob es darum geht, ein Datum auf einem Kalender anzuzeigen oder zu prüfen, ob eine Aufgabe rechtzeitig erledigt wurde, die Fähigkeit, das aktuelle Datum in einer Anwendung zu erhalten, ist von großer Bedeutung.

## How To

Um das aktuelle Datum in Swift zu bekommen, gibt es verschiedene Möglichkeiten. Eine einfache Möglichkeit ist die Verwendung der Date-Klasse mit der statischen Methode `init()`, die standardmäßig das aktuelle Datum zurückgibt:

```Swift
let date = Date()
print(date) //Output: 2020-09-23 15:30:21 +0000
```

Eine andere Möglichkeit ist die Verwendung des DateFormatters, um das Datum in einem bestimmten Format zurückzugeben:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let dateString = formatter.string(from: Date())
print(dateString) //Output: 23.09.2020
```

Man kann auch das aktuelle Datum abfragen, indem man eine Instanz der `Calendar`-Klasse verwendet:

```Swift
let calendar = Calendar.current
let date = calendar.dateComponents([.year, .month, .day], from: Date())
print("\(date.year!)-\(date.month!)-\(date.day!)") //Output: 2020-9-23
```

## Deep Dive

Die `Date`-Klasse in Swift ist eine Wrapper-Klasse für die Foundation-Klasse `NSDate`, die in Objective-C verwendet wird. Diese Klasse stellt ein Datum in der Vergangenheit oder Zukunft dar, indem sie die Anzahl der Sekunden seit dem 1. Januar 2001 um 00:00:00 Uhr UTC speichert. Mit anderen Worten, sie repräsentiert ein bestimmtes Datum als Zeitintervall und kann Werte vergleichen, addieren, subtrahieren und konvertieren.

Um das aktuelle Datum in Swift zu bekommen, wird die `Date`-Klasse verwendet, die wiederum ein Objekt von `NSDate` zurückgibt. Dieses Objekt enthält die Informationen über das aktuelle Datum, das in einer bestimmten Zeitzone angezeigt wird. Die `Date`-Klasse bietet auch Methoden, um das Datum in verschiedene Formate umzuwandeln oder um bestimmte Komponenten (Jahr, Monat, Tag usw.) abzurufen.

## Siehe auch

- [Swift Dokumentation zu Date](https://developer.apple.com/documentation/foundation/date)
- [NSCalendar Klasse von Apple](https://developer.apple.com/documentation/foundation/nscalendar)
- [Tutorial zu Swift Date and Time](https://medium.com/swift-india/swift-date-and-time-beginners-guide-7878ed9d4695)