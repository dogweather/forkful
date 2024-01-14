---
title:                "Swift: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

In der Programmierung ist es oft notwendig, zwei Daten miteinander zu vergleichen, um bestimmte Aktionen auszuführen oder Bedingungen zu überprüfen. In der Swift-Programmierung gibt es verschiedene Möglichkeiten, um die Vergleichbarkeit von Daten, insbesondere von Datumswerten, zu implementieren. In diesem Blog-Beitrag werden wir uns ansehen, wie man effektiv zwei Datumswerte vergleichen kann.

## Wie man zwei Datumswerte vergleicht

Um zwei Datumswerte in Swift zu vergleichen, gibt es zwei Optionen: die Verwendung des >- oder < -Operators oder die Verwendung der compare-Methode des Date-Typs. Im Folgenden werden wir beide Optionen in Aktion sehen.

### Verwendung von >- oder < -Operators

Um zu überprüfen, ob ein Datum vor oder nach einem anderen Datum liegt, können wir den >- oder < -Operator verwenden. Hier ist ein Beispielcode, der dies demonstriert:

```Swift
let date1 = Date() // aktuelles Datum
let date2 = Date(timeIntervalSinceNow: -86400) // Datum von gestern
if date1 > date2 {
   print("Das aktuelle Datum ist später als das Datum von gestern.")
}
```

Die Ausgabe wäre wie folgt:

```Swift
Das aktuelle Datum ist später als das Datum von gestern.
```

### Verwendung der compare-Methode

Eine andere Möglichkeit, zwei Datumswerte in Swift zu vergleichen, besteht darin, die compare-Methode des Date-Typs zu verwenden. Diese Methode gibt einen Enumerationstyp zurück, der die Ergebnisbeziehung zwischen den beiden Datumswerten darstellt: entweder aufsteigend, absteigend oder gleich. Schauen wir uns ein Beispiel an:

```Swift
let date1 = Date(timeIntervalSince1970: 1577836800) // 01.01.2020
let date2 = Date(timeIntervalSince1970: 1577836800) // 01.01.2020
let comparisonResult = date1.compare(date2) // die beiden sind gleich
print(comparisonResult == .orderedSame) //true
```

In diesem Beispiel sind beide Datumswerte identisch, daher gibt die compare-Methode .orderedSame zurück, was darauf hinweist, dass sie in der gleichen Reihenfolge sind.

## Tiefere Einblicke

Beim Vergleich von Datumswerten in Swift gibt es einige wichtige Dinge zu beachten. Zum Beispiel können Datumswerte auch die Zeit enthalten, was bedeutet, dass zwei Datumswerte, obwohl sie auf demselben Tag liegen, möglicherweise nicht in der gleichen Reihenfolge sind, wenn sie unterschiedliche Zeiten enthalten. Darüber hinaus kann die Verwendung von Zeitbereichen wie Zeitzonen oder Sommerzeit die Ergebnisse des Vergleichs beeinflussen. Es ist daher wichtig, bei der Implementierung von Datumswertvergleichen genau zu sein und mögliche Auswirkungen von Zeitänderungen zu berücksichtigen.

# Siehe auch

- [Offizielle Dokumentation von Swift zu Datumswertvergleichen](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID540)
- [Tutorial zu Datumswertvergleichen in Swift von Coding Explorer](https://www.codingexplorer.com/comparing-dates-swift/)