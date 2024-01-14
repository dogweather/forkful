---
title:                "Kotlin: Vergleich von zwei Datumsangaben"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten kann eine nützliche Funktion in der Programmierung sein, da es ermöglicht, Daten zu sortieren, zu filtern oder auch Bedingungen zu überprüfen. Das spart Zeit und macht den Code effizienter.

## Wie geht das?

Um zwei Daten in Kotlin zu vergleichen, können wir die Funktion `compareTo()` verwenden. Diese Methode gibt einen Integer zurück, der angibt, ob das erste Datum früher, später oder gleich dem zweiten Datum ist. Beispiel:

```Kotlin
val date1 = LocalDate.parse("2020-01-01")
val date2 = LocalDate.parse("2020-03-01")

println(date1.compareTo(date2)) // Output: -1
```

In diesem Beispiel wird das Datum `date1` als früher als das Datum `date2` betrachtet, da der Output der `compareTo()` Funktion `-1` ist. Wenn die Daten gleich sind, wird `0` ausgegeben und wenn die zweite Datum früher ist, wird `1` ausgegeben.

Weitere Vergleichsoperationen können auch mit den Vergleichsoperatoren `>`, `<`, `>=` und `<=` durchgeführt werden. Beispiel:

```Kotlin
if (date1 > date2) {
   println("Das Datum 1 ist später als Datum 2.")
}
```

## Tiefer eintauchen

Beim Vergleichen von Daten ist es wichtig, darauf zu achten, dass bestimmte Datumsformate wie `LocalDate` oder `LocalDateTime` in Kotlin nativ vergleichbar sind. Andere Datumsformate wie `Date` müssten zunächst in ein kompatibles Format umgewandelt werden.

Ein weiterer wichtiger Punkt ist, dass bei der Verwendung von `compareTo()` immer das erste Datum als Referenz verwendet wird. Es ist daher wichtig, die Datumsreihenfolge im Auge zu behalten, um unerwünschte Ergebnisse zu vermeiden.

## Siehe auch

- [Offizielle Kotlin-Dokumentation](https://kotlinlang.org/docs/reference/comparison-operations.html)
- [Vergleichsoperatoren in Kotlin](https://kotlinlang.org/docs/reference/comparison.html)
- [Tutorial zum Vergleichen von Daten in Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_basic_operators.htm)