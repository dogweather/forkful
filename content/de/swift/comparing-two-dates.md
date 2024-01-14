---
title:    "Swift: Zwei Daten vergleichen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von Datumsangaben ist ein wichtiger Bestandteil der Entwicklung von iOS Apps mit Swift. Es ermöglicht uns, bestimmte Aktionen basierend auf dem Datum auszulösen oder sogar benutzerdefinierte Sortierfunktionen zu implementieren. In diesem Blog-Beitrag zeigen wir Ihnen, wie Sie effektiv zwei Datumsangaben in Swift vergleichen können.

## Wie man es macht

Um zwei Datumsangaben in Swift zu vergleichen, müssen wir zuerst zwei Variablen vom Typ `Date` erstellen. Das können wir entweder direkt mit einem Wert in der Vergangenheit oder Zukunft tun oder indem wir eine Funktion wie `Date()` verwenden, die das aktuelle Datum und die aktuelle Uhrzeit zurückgibt.

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceReferenceDate: 10000)
```
Wir können jetzt `date1` und `date2` mit dem Vergleichsoperator `>` oder `<` vergleichen, um zu sehen, welches Datum später oder früher ist.

```Swift
if date1 > date2 {
    print("date1 ist später als date2")
} else if date1 < date2 {
    print("date2 ist später als date1")
} else {
    print("beide Datumsangaben sind gleich")
}
```

Die Ausgabe dieses Codes wird je nach Datum, das Sie für `date2` gewählt haben, variieren. Wenn das Datum von `date2` in der Vergangenheit liegt, sollte die Ausgabe "date2 ist später als date1" lauten.

## Tiefergehende Informationen

Bei der Verwendung von Date in Swift gibt es einige Dinge zu beachten, die zu unerwartetem Verhalten führen können. Zum Beispiel berücksichtigt Swift bei der Vergleichsoperation die Millisekunden, was dazu führen kann, dass zwei eigentlich gleiche Datumsangaben als unterschiedlich betrachtet werden. Um dies zu vermeiden, sollten wir die Funktion `timeIntervalSince1970` verwenden, um die Datumsangabe in Sekunden zu konvertieren.

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinc1970: date1.timeIntervalSince1970)
```

Jetzt sollten `date1` und `date2` unabhängig davon, wie viele Millisekunden sie voneinander abweichen, als gleich betrachtet werden.

## Siehe auch

- [Apple Dokumentation zu Date](https://developer.apple.com/documentation/foundation/date)
- [Stack Overflow Beitrag zum Vergleichen von Datum und Uhrzeit in Swift](https://stackoverflow.com/questions/25017773/comparing-two-dates-in-swift)