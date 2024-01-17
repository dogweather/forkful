---
title:                "Vergleich von zwei Daten"
html_title:           "Kotlin: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Programmieren ist es oft wichtig, zwei verschiedene Daten zu vergleichen. Das bedeutet, dass wir überprüfen wollen, ob zum Beispiel ein Datum in der Zukunft oder Vergangenheit liegt oder ob zwei Termine auf den gleichen Tag fallen. Programmierer nutzen diese Vergleiche, um Daten zu sortieren oder um Bedingungen zu setzen.

## So geht's:

Für die Vergleich von zwei Daten stehen in Kotlin verschiedene Methoden zur Verfügung. Die gebräuchlichste Methode ist die Verwendung des "filter"-Methoden in Kombination mit "compareTo". Diese beiden Methoden können in einer Codezeile verwendet werden, um die Bedingungen für die Vergleiche zu setzen.

```Kotlin
val date1 = LocalDate.of(2021, 10, 31) // Das Datum kann beliebig eingestellt werden
val date2 = LocalDate.now() // Hier wird das heutige Datum verwendet
val comparison = date2.compareTo(date1)
if (comparison == 0) { // Wenn die beiden Daten gleich sind
    println("Die Daten sind gleich.")
} else if (comparison > 0) { // Wenn date2 später ist als date1
    println("date2 ist später als date1.")
} else { // Wenn date2 früher ist als date1
    println("date2 ist früher als date1.")
}
```

Die Ausgabe wird je nach den gewählten Daten unterschiedlich sein. Wenn das heutige Datum in unserem Beispiel den 31. Oktober 2021 ist, wird die Ausgabe "Die Daten sind gleich." sein. Wenn das heutige Datum jedoch beispielsweise der 1. November 2021 ist, wird die Ausgabe "date2 ist später als date1." sein.

## Tiefenschärfe:

In der Vergangenheit gab es verschiedene Möglichkeiten, Daten in Programmen zu vergleichen. Eine davon war die Verwendung von "Date" Objekten, die jedoch oft aufgrund der Zeitzone oder anderer Faktoren zu inkonsistenten Ergebnissen führten. Mit der Einführung von Java 8 wurde die "LocalDate" Klasse eingeführt, die derzeit die bevorzugte Methode in Kotlin ist, um Daten zu vergleichen.

Alternativ kann auch die "isAfter" oder "isBefore" Methoden verwendet werden, um festzustellen, ob ein Datum später oder früher als ein anderes ist. Diese Methoden funktionieren ähnlich wie "compareTo", liefern jedoch ein boolean als Ergebnis.

## Siehe auch:

- [Offizielle Kotlin Dokumentation zu Dates](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [Vergleich von Daten mit Java 8](https://docs.oracle.com/javase/tutorial/datetime/iso/whythreedten.html)
- [Vergleich von Daten mit anderen Programmiersprachen](https://www.geeksforgeeks.org/date-class-in-java/)