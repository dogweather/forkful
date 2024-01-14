---
title:                "Kotlin: Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit kann ein nützliches Werkzeug sein, um z.B. Fristen oder Ereignisse zu planen oder auf vergangene Ereignisse zurückzublicken.

## Wie geht es

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die `LocalDate` Klasse aus der `java.time` library in Kotlin verwenden. Zuerst müssen wir das heutige Datum mit `LocalDate.now()` erhalten. Von dort aus können wir mithilfe der `minus()` und `plus()` Funktionen die gewünschte Anzahl von Tagen, Monaten oder Jahren hinzufügen oder subtrahieren.

```kotlin
val today = LocalDate.now()

//Berechnen eines Datums 10 Tage in der Zukunft
val futureDate = today.plusDays(10)

//Berechnen eines Datums 1 Jahr in der Vergangenheit
val pastDate = today.minusYears(1) 
```

Die Ausgabe für `futureDate` wäre dann z.B. `2037-06-16` falls das heutige Datum der 6. Juni 2037 ist.

## Tiefer eintauchen

Es gibt viele Möglichkeiten, mit Datum-Berechnungen in Kotlin zu spielen. Eine davon ist die Verwendung von `ChronoUnit` um die Differenz zwischen zwei Daten zu berechnen. Hier ist ein Beispiel, das die Anzahl von Tagen zwischen dem heutigen Datum und einem zukünftigen Datum in einer benutzerfreundlichen Nachricht ausgibt.

```kotlin
val days = ChronoUnit.DAYS.between(today, futureDate)

println("In $days Tagen " + "ist der $futureDate")
```

Die Ausgabe dieser Code-Segmente wäre `In 10 Tagen ist der 2037-06-16`.

## Siehe auch

- [Offizielle Kotlin Dokumentation für die `java.time` library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time/-local-date/index.html)
- [Kotlin Playground mit Beispielen für Datum-Berechnungen](https://play.kotlinlang.org/)