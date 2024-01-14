---
title:    "Kotlin: Berechnen eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Warum

Das Berechnen von Datum in der Vergangenheit oder Zukunft kann hilfreich sein, um Datumsangaben in Programmen zu manipulieren oder um zukünftige Ereignisse vorherzusagen.

# Wie

Die Bibliothek `java.time` bietet in Kotlin eine einfache Möglichkeit, um Datum und Zeit zu manipulieren. Im folgenden Beispiel wird gezeigt, wie man Datumsangaben in der Vergangenheit oder Zukunft berechnen kann:

```Kotlin
import java.time.LocalDate
import java.time.Month

fun main() {
    // aktuelles Datum
    val today = LocalDate.now()
    println("Heute: $today")

    // Datum in der Zukunft
    val futureDate = today.plusDays(7)
    println("In 7 Tagen: $futureDate")

    // Datum in der Vergangenheit
    val pastDate = today.minusMonths(3)
    println("Vor 3 Monaten: $pastDate")

    // ein bestimmtes Datum setzen
    val specificDate = LocalDate.of(2020, Month.DECEMBER, 25)
    println("Weihnachten 2020: $specificDate")
}
```

Das obige Beispiel würde diese Ausgabe erzeugen:

```
Heute: 2020-10-22
In 7 Tagen: 2020-10-29
Vor 3 Monaten: 2020-07-22
Weihnachten 2020: 2020-12-25
```

# Deep Dive

Bei der Berechnung von Datum in der Vergangenheit oder Zukunft gibt es einige wichtige Dinge zu beachten. Zum Beispiel können einige Monate unterschiedliche Anzahl von Tagen haben, wodurch die Berechnung von Datum etwas komplizierter werden kann. Außerdem kann es aufgrund von Schaltjahren Unterschiede in der Anzahl der Tage geben. Es ist daher wichtig, die Funktionen der `java.time` Bibliothek genau zu verstehen, um genaue Berechnungen durchzuführen.

# Siehe auch

- [Offizielle Dokumentation zu java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Kotlin Programmierleitfaden](https://kotlinlang.org/docs/reference/basic-syntax.html)
- [Einführung in die Zeitrechnung mit java.time](https://www.baeldung.com/java-8-date-time-intro)