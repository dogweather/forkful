---
title:                "Ein Datum in der Zukunft oder Vergangenheit berechnen"
html_title:           "Kotlin: Ein Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Ein Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Berechnen eines Datums in der Zukunft oder Vergangenheit beinhaltet das Hinzufügen oder Subtrahieren von Tagen, Wochen, Monaten oder Jahren von einem gegebenen Datum, um ein neues gewünschtes Datum zu erhalten. Programmierer tun dies, um zukünftige Ereignisse zu planen oder zu überprüfen, wann ein bestimmtes Ereignis in der Vergangenheit stattgefunden hat.

## Wie geht's?

Die Berechnung eines Datums in der Zukunft oder Vergangenheit kann in Kotlin mithilfe der "add" und "subtract" Funktionen aus der Klasse "kotlin.time.LocalDate" durchgeführt werden. Hier ist ein Beispielcode, der zuerst 5 Tage zu einem aktuellen Datum hinzufügt und dann 2 Monate davon subtrahiert:

```Kotlin 
val currentDate = LocalDate.now()
val futureDate = currentDate.add(Duration.days(5))
val pastDate = currentDate.subtract(Duration.months(2))

println("Heute: $currentDate")
println("In 5 Tagen: $futureDate")
println("Vor 2 Monaten: $pastDate")
```
Ausgabe:
```
Heute: 2021-11-10
In 5 Tagen: 2021-11-15
Vor 2 Monaten: 2021-09-10
```

## Tiefer eintauchen

Die Berechnung von Daten in der Zukunft oder Vergangenheit ist in der Programmierung wichtig, um zu planen, wann bestimmte Aktionen ausgeführt werden sollen oder um zu überprüfen, wann Ereignisse in der Vergangenheit stattgefunden haben. Alternativ können auch Bibliotheken wie "java.time" oder "Joda-Time" verwendet werden, um Daten in Java zu berechnen.

Es ist wichtig zu beachten, dass bei der Berechnung von Daten in unterschiedlichen Zeitzonen das Ergebnis je nach implementierter Methode variieren kann. Daher ist es ratsam, eine Methode zu wählen, die die gewünschten Ergebnisse erzielt und die Zeitzonenkorrektur berücksichtigt.

## Siehe auch

- Kotlin Dokumentation für "LocalDate": https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/
- Java Dokumentation für "java.time": https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Joda-Time Dokumentation: https://www.joda.org/joda-time/