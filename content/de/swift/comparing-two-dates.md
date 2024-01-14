---
title:    "Swift: Vergleich von zwei Daten"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

Warum Vergleichen von zwei Daten?

Das Vergleichen von zwei Daten ist ein grundlegender Bestandteil der Programmierung, besonders wenn es um die Verarbeitung von Datums- und Zeitinformationen geht. Durch das Vergleichen von zwei Daten können EntwicklerInnen die Reihenfolge von Ereignissen bestimmen, zeitbasierte Funktionen erstellen und vieles mehr.

Wie geht man vor?

Um zwei Daten zu vergleichen, gibt es in Swift verschiedene Möglichkeiten. Eine Möglichkeit ist die Verwendung von Vergleichsoperatoren wie "<", ">", "<=" und ">=". Diese operieren auf den Werten der Daten und geben einen Wahrheitswert zurück, der angibt, ob die erste Daten größer, kleiner oder gleich der zweiten Daten ist. Zum Beispiel:

```
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: -86400) // ein Tag vorher
if date1 > date2 {
    print("Date 1 ist später als Date 2")
} else if date1 < date2 {
    print("Date 1 ist früher als Date 2")
} else {
    print("Die Daten sind gleich")
}
```

Eine weitere Option ist die Verwendung von Vergleichsmethoden auf Datenobjekten wie `compare(_:)`, die eine `ComparisonResult`-Enumeration zurückgeben. Diese enthält die Fälle `orderedAscending`, `orderedDescending` und `orderedSame`, die die Vergleichsbeziehung zwischen den Daten angeben. Zum Beispiel:

```
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: -86400) // ein Tag vorher
let comparison = date1.compare(date2)
switch comparison {
case .orderedAscending:
    print("Date 1 liegt vor Date 2")
case .orderedDescending:
    print("Date 1 liegt nach Date 2")
case .orderedSame:
    print("Die Daten sind gleich")
}
```

Tiefere Einblicke:

Beim Vergleichen von zwei Daten ist es wichtig zu beachten, dass es zwei verschiedene Arten von Daten gibt: `Date`-Objekte und `DateComponents`-Objekte. `Date`-Objekte repräsentieren einen bestimmten Moment in der Zeit, während `DateComponents`-Objekte spezifische Zeitinformationen wie Jahr, Monat, Tag usw. darstellen.

Beim Vergleichen von `Date`-Objekten gibt es keine Probleme, da sie anhand ihrer zugrundeliegenden Zeitstempel verglichen werden. Bei `DateComponents`-Objekten hingegen kann es zu Problemen kommen, da sie keine vollständigen Datums- und Zeitinformationen enthalten können. Daher ist es wichtig, die `Calendar`-Klasse zu verwenden, um die `DateComponents`-Objekte in `Date`-Objekte umzuwandeln, bevor sie verglichen werden. Zum Beispiel:

```
let date1 = Date()
var date2Components = DateComponents()
date2Components.day = 21
date2Components.month = 5
date2Components.year = 2021
let date2 = Calendar.current.date(from: date2Components) // Erstellt ein Date-Objekt aus den DateComponents
if let date2 = date2 {
    if date1 > date2 {
        print("Date 1 liegt später als Date 2")
    } else if date1 < date2 {
        print("Date 1 liegt früher als Date 2")
    } else {
        print("Die Daten sind gleich")
    }
}
```

Siehe auch:

- [Apple Dokumentation über `Date`](https://developer.apple.com/documentation/foundation/date): Verwenden Sie diese Dokumentation, um mehr über `Date`-Objekte zu erfahren.
- [Apple Dokumentation über `DateComponents`](https://developer.apple.com/documentation/foundation/datecomponents): Verwenden Sie diese Dokumentation, um mehr über `DateComponents`-Objekte zu erfahren.
- [Apple Dokumentation über `Calendar`](https://developer.apple.com/documentation/foundation/calendar): Verwenden Sie diese Dokumentation, um mehr über die Verwendung von `Calendar` bei der Arbeit mit Datums- und Zeitinformationen zu erfahren.