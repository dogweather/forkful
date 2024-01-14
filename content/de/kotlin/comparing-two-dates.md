---
title:                "Kotlin: Vergleich zweier Daten"
simple_title:         "Vergleich zweier Daten"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Datumsangaben ist eine häufige Aufgabe in der Programmierung. Es ermöglicht uns, zu überprüfen, ob ein bestimmtes Datum vor oder nach einem anderen Datum liegt und kann in vielen Anwendungen nützlich sein.

## Wie geht man vor

Das Vergleichen von zwei Daten in Kotlin ist einfach und unkompliziert. Wir können die `before()` oder `after()` Funktionen der `LocalDate` Klasse verwenden, um die Reihenfolge der Datumsangaben zu überprüfen.

Beispielcode:

```Kotlin
val date1 = LocalDate.of(2020, 10, 15)
val date2 = LocalDate.of(2020, 10, 20)

if(date1.before(date2)){
    println("$date1 ist vor $date2")
} else{
    println("$date2 ist vor $date1")
}
```

Output:

```
2020-10-15 ist vor 2020-10-20
```

Wir können auch die `isEqual()` Funktion verwenden, um zu überprüfen, ob zwei Datumsangaben gleich sind.

Beispielcode:

```Kotlin
val date1 = LocalDate.of(2020, 10, 15)
val date2 = LocalDate.of(2020, 10, 15)

if(date1.isEqual(date2)){
	println("$date1 und $date2 sind gleich")
} else{
	println("$date1 und $date2 sind unterschiedlich")
}
```

Output:

```
2020-10-15 und 2020-10-15 sind gleich
```

## Tiefergehende Informationen

Bei der Verwendung von `before()` und `after()` Funktionen wird der Vergleich auf der Grundlage des Kalendersystems durchgeführt. Es ist wichtig sicherzustellen, dass beide Datumsangaben im selben Kalendersystem (z.B. Gregorianischer Kalender oder Julianischer Kalender) sind, um genaue Ergebnisse zu erhalten.

Es ist auch möglich, die Reihenfolge der Datumsangaben basierend auf bestimmten Kriterien zu überprüfen, z.B. das Vergleichen von Jahr, Monat und Tag getrennt. Hierfür können wir die `compareTo()` Funktion verwenden.

Beispielcode:

```Kotlin
val date1 = LocalDate.of(2020, 10, 15)
val date2 = LocalDate.of(2020, 10, 20)

if(date1.compareTo(date2) < 0 ){
	println("$date1 ist vor $date2")
} else if(date1.compareTo(date2) > 0){
	println("$date1 ist nach $date2")
} else{
	println("$date1 und $date2 sind gleich")
}
```

Output:

```
2020-10-15 ist vor 2020-10-20
```

## Siehe auch

- [Java 8: Vergleichen von Datumsangaben](https://www.baeldung.com/java-compare-dates)
- [Kotlin Dokumentation: Lokale Datumsangaben](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)