---
title:                "Zwei Daten vergleichen"
html_title:           "Kotlin: Zwei Daten vergleichen"
simple_title:         "Zwei Daten vergleichen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit dem Vergleich von zwei Daten beschäftigen? Nun, wenn du jemals ein Programm geschrieben hast, das mit Datumswerten arbeitet, wirst du wissen, dass es manchmal notwendig ist, sie miteinander zu vergleichen. Zum Beispiel, um herauszufinden, welches Ereignis zuerst stattgefunden hat oder ob ein Datum innerhalb eines bestimmten Zeitraums liegt. In diesem Artikel werden wir uns ansehen, wie man dies mit Kotlin machen kann.

## Wie geht das?

Um zwei Daten in Kotlin zu vergleichen, können wir die `compareTo()` Methode der `Date` Klasse verwenden. Schauen wir uns ein Beispiel an:

```Kotlin
val date1 = Date(2021, 7, 10) // Erstes Datum
val date2 = Date(2021, 7, 15) // Zweites Datum
val result = date1.compareTo(date2) // Vergleiche beide Daten

println(result) // Output: -5
```

Das `compareTo()` gibt eine ganzzahlige Zahl zurück, die angibt, ob das erste Datum vor, gleich oder nach dem zweiten Datum liegt. In diesem Beispiel beträgt die Ausgabe "-5", was bedeutet, dass das erste Datum vor dem zweiten liegt. Wenn das erste Datum nach dem zweiten Datum liegen würde, wäre die Ausgabe "5" und falls beide Daten gleich sind, wäre die Ausgabe "0".

Wir können auch die `before()` und `after()` Methoden verwenden, wenn wir nur prüfen möchten, ob ein Datum vor oder nach einem anderen liegt, ohne die genauen Unterschiede in der Zeit zu kennen.

```Kotlin
val date1 = Date(2021, 7, 10) // Erstes Datum
val date2 = Date(2021, 7, 15) // Zweites Datum

if (date1.before(date2)) {
    println("Das erste Datum liegt vor dem zweiten.")
}

if (date2.after(date1)) {
    println("Das zweite Datum liegt nach dem ersten.")
}
```

## Tiefere Einblicke

Die Vergleichsmethoden der `Date` Klasse vergleichen die Daten auf Millisekundenebene. Das bedeutet, dass selbst kleinste Unterschiede in der Zeit zu unterschiedlichen Rückgabewerten führen können. Es ist auch wichtig zu beachten, dass die `Date` Klasse in Kotlin veraltet ist und durch die `LocalDate` Klasse ersetzt wurde. Diese bietet eine bessere Unterstützung für Datumsberechnungen und Vergleiche.

## Siehe auch

- [Kotlin Dokumentation zu Datumsvergleichen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/compare-to.html)
- [Offizielle Kotlin Dokumentation](https://kotlinlang.org/)
- [Kotlin Tutorials und Beispiele](https://www.programiz.com/kotlin)