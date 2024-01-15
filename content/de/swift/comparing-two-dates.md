---
title:                "Zwei Daten vergleichen"
html_title:           "Swift: Zwei Daten vergleichen"
simple_title:         "Zwei Daten vergleichen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Die Vergleichung von zwei Daten ist eine häufige Aufgabe in der Programmierung. Mit Swift kann dies mithilfe von integrierten Funktionen schnell und einfach durchgeführt werden. In diesem Artikel erfahren Sie, wie Sie zwei Daten vergleichen und warum dies nützlich sein kann.

## Wie geht das?

Um zwei Daten in Swift zu vergleichen, gibt es verschiedene Möglichkeiten. Eine Möglichkeit ist die Verwendung der `compare()` Funktion, die in der Klasse `Date` enthalten ist. Schauen wir uns ein Beispiel an:

```Swift
let date1 = Date()
let date2 = Date().addingTimeInterval(60) // 60 Sekunden (= 1 Minute) später

if date1.compare(date2) == .orderedAscending {
    print("\(date1) liegt vor \(date2)")
} else if date1.compare(date2) == .orderedDescending {
    print("\(date1) liegt nach \(date2)")
} else {
    print("\(date1) ist gleich \(date2)")
}

// Ausgabe: 2021-03-25 10:00:00 liegt vor 2021-03-25 10:01:00
```

In diesem Beispiel verwenden wir die `compare()` Funktion, um zu prüfen, ob `date1` vor oder nach `date2` liegt, oder ob beide Daten gleich sind. Wir können auch die `earlier()` und `later()` Funktionen verwenden, um das frühere bzw. spätere Datum zu erhalten.

Eine weitere Möglichkeit ist die Verwendung von Operatoren (`<`, `>`, `==`), um zwei Daten zu vergleichen. Dies kann besonders praktisch sein, wenn Sie nur überprüfen möchten, ob eine Bedingung erfüllt ist, ohne eine bestimmte Ausgabe auszuführen. Schauen wir uns ein Beispiel an:

```Swift
let yesterday = Date().addingTimeInterval(-86400) // 86400 Sekunden (= 1 Tag) früher

if yesterday < Date() {
    print("Gestern war früher als heute.")
}

// Ausgabe: Gestern war früher als heute.
```

Hier vergleichen wir `yesterday`, das einen Tag früher als das aktuelle Datum ist, mit `Date()` mithilfe des `<` Operators. Wenn die Bedingung erfüllt ist, wird die Ausgabe ausgeführt. 

Es ist auch möglich, zwei Daten mithilfe von `timeIntervalSince(_:)` zu vergleichen, um die Differenz zwischen den beiden Daten in Sekunden zu erhalten. Wenn die Differenz größer als `0` ist, ist das erste Datum später als das zweite. Ansonsten ist das erste Datum früher als das zweite. Schauen wir uns ein Beispiel an:

```Swift
let currentTime = Date()
let fiveMinutesLater = Date().addingTimeInterval(300) // 300 Sekunden (= 5 Minuten) später

if currentTime.timeIntervalSince(fiveMinutesLater) > 0 {
    print("\(currentTime) ist später als \(fiveMinutesLater)")
} else {
    print("\(currentTime) ist früher als \(fiveMinutesLater)")
}

// Ausgabe: 2021-03-25 10:00:00 ist früher als 2021-03-25 10:05:00
```

## Tieferer Einblick

Beim Vergleichen von zwei Daten ist es wichtig zu beachten, dass die Ergebnisse von verschiedenen Faktoren wie Zeitzone, Kalender und Locale beeinflusst werden können. Es ist daher wichtig, sicherzustellen, dass beide Daten auf die gleiche Weise interpretiert werden. Hierfür gibt es die `compare(_:toGranularity:)` Funktion, die die Genauigkeit des Vergleichs bestimmt. Wir können zwischen fünf verschiedenen Granularitätsstufen wählen: `.era`, `.year`, `.month`, `.day` und `.minute`. Ein höherer Granularitätsgrad bedeutet, dass auch kleinere Unterschiede, wie z.B. die Sekunden, berücksichtigt werden.

Ein weiterer wichtiger Aspekt beim Vergleichen von Daten ist die Verwendung von Zeitintervallen. Sie können die Funktion `addingTimeInterval(_:)` verwenden, um ein Datum um eine bestimmte Zeit zu ändern. Diese Funktion ist besonders hilfreich beim Umgang mit unterschiedlichen Zeitzonen oder bei der Durchführung von Berechnungen auf Basis von Zeiträumen.

## Siehe auch

- [Apple Dokumentation zu Date](https://developer.apple.com/documentation/foundation/date)
- [Weitere Vergleichsoperatoren in Swift](https://docs.swift.org/swift-book