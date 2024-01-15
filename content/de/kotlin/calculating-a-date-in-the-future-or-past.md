---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Kotlin: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Terminen kann in vielen Programmieraufgaben nützlich sein, z.B. für die zeitbasierte Planung von Aufgaben oder die Darstellung von Ereignissen in einer Anwendung.

## Anleitung

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die `LocalDate` Klasse aus der `java.time` Bibliothek verwenden.

Um z.B. das Datum von heute um 2 Monate in der Zukunft zu erhalten, können wir folgenden Code verwenden:

```Kotlin
val currentDate = LocalDate.now()
val futureDate = currentDate.plusMonths(2)
println("Das Datum in zwei Monaten wird sein: $futureDate")
```

Die Ausgabe dieses Codes wäre: `Das Datum in zwei Monaten wird sein: 2021-08-26`.

Um ein Datum in der Vergangenheit zu berechnen, können wir stattdessen die `minus` Methode verwenden, z.B.:

```Kotlin
val pastDate = currentDate.minusWeeks(1)
println("Das Datum vor einer Woche war: $pastDate")
```

Die Ausgabe dieses Codes wäre: `Das Datum vor einer Woche war: 2021-07-12`.

## Tiefere Einblicke

Es gibt auch eine Reihe von anderen Methoden in der `LocalDate` Klasse, die helfen können, komplexe Datumsberechnungen durchzuführen. Zum Beispiel können wir die `withDayOfYear` Methode verwenden, um das Datum auf einen bestimmten Tag im Jahr zu setzen:

```Kotlin
val birthday = LocalDate.now().withDayOfYear(200)
println("Mein Geburtstag ist am: $birthday")
```

Die Ausgabe dieses Codes wäre: `Mein Geburtstag ist am: 2021-07-19`.

Außerdem können wir die `until` Methode verwenden, um die Anzahl der Tage zwischen zwei Datumswerten zu berechnen, z.B.:

```Kotlin
val christmas = LocalDate.of(2021, Month.DECEMBER, 24)
val daysUntilChristmas = currentDate.until(christmas, ChronoUnit.DAYS)
println("Es sind noch $daysUntilChristmas Tage bis Weihnachten!")
```

Die Ausgabe dieses Codes wäre: `Es sind noch 153 Tage bis Weihnachten!`.

## Siehe auch

- [Offizielle Kotlin Dokumentation zu Date and Time](https://kotlinlang.org/docs/dart-overview.html)
- [Java LocalDate Dokumentation] (https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)