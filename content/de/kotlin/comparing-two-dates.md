---
title:    "Kotlin: Vergleich von zwei Daten"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals Daten in Ihrem Code vergleichen mussten, wissen Sie wahrscheinlich, wie mühsam und fehleranfällig es sein kann. Die gute Nachricht ist jedoch, dass Kotlin eine Reihe von Funktionen bietet, die das Vergleichen von Daten viel einfacher und sicherer machen. In diesem Artikel werden wir uns genauer ansehen, wie man in Kotlin zwei Daten vergleichen kann.

## Wie geht's

Um zwei Daten in Kotlin zu vergleichen, gibt es verschiedene Möglichkeiten. Eine davon ist die Nutzung der "compareTo" Methode, die es uns ermöglicht, das Ergebnis des Vergleichs zwischen zwei Daten als Ganzzahl zurückzugeben. Das folgende Beispiel zeigt, wie man diese Methode verwendet:

```Kotlin
val date1 = LocalDate.of(2021, 1, 1)
val date2 = LocalDate.of(2021, 1, 5)
val result = date1.compareTo(date2)

println(result) // Output: -4
```

In diesem Beispiel wird das Ergebnis -4 zurückgegeben, da date1 vor date2 liegt. Wenn die beiden Daten gleich sind, wird 0 zurückgegeben und wenn das erste Datum nach dem zweiten liegt, wird 4 zurückgegeben.

Eine andere Möglichkeit, um zwei Daten zu vergleichen, ist die Verwendung des "isBefore" und "isAfter" Operators. Diese Methode gibt einfach einen booleschen Wert zurück, der angibt, ob das erste Datum vor oder nach dem zweiten Datum liegt. Das folgende Beispiel zeigt, wie man diese Operatoren verwendet:

```Kotlin
val date1 = LocalDate.of(2021, 1, 1)
val date2 = LocalDate.of(2021, 1, 5)

println(date1.isBefore(date2)) // Output: true
println(date1.isAfter(date2)) // Output: false
```

Hier wird für das erste Datum "true" zurückgegeben, da es vor dem zweiten Datum liegt.

## Tiefere Einblicke

Bei der Verwendung von Daten in Java müssen wir häufig auf die Zeitzone und den Kalender achten, um genaue Vergleiche durchzuführen. Glücklicherweise kümmert sich Kotlin um diese Details und macht das Vergleichen von Daten wesentlich einfacher. In Kotlin gibt es die Klasse "LocalDate", die ein Datum ohne Zeitzoneinformationen darstellt. Diese Klasse ist in der Regel ausreichend, um zwei Daten zu vergleichen. Wenn Sie jedoch Datum mit Zeitzoneinformationen benötigen, gibt es auch die Klasse "ZonedDateTime", die dieses Feature bietet.

Ein weiterer wichtiger Punkt beim Vergleichen von Daten ist die Berücksichtigung von Schaltjahren. In Kotlin gibt es die praktische Methode "isLeapYear()", die es uns ermöglicht, ganz einfach herauszufinden, ob ein bestimmtes Jahr ein Schaltjahr ist oder nicht.

## Siehe auch

- [Kotlin Dokumentation zu Datum und Zeit](https://kotlinlang.org/docs/datetime.html)
- [Vergleich von zwei Zeiten in Java](https://www.baeldung.com/java-compare-dates)
- [Das ist neu in Kotlin 1.4](https://blog.jetbrains.com/de/2020/08/kotlin-1-4-released-with-simultaneous-jvm-and-js-release/)