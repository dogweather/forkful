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

## Was & Warum?

Das Berechnen eines zukünftigen oder vergangenen Datums ist die Methode, mit der wir herausfinden, wie ein bestimmtes Datum in der Vergangenheit oder Zukunft aussieht. Programmierer machen das oft, um Berichte zu erstellen, Ereignisse zu planen oder Zeitstempel zu verfolgen.

## So geht's: 

Lassen Sie uns sehen, wie man mit Kotlin ein Datum in der Zukunft oder Vergangenheit berechnen kann. Wir werden die `java.time` API verwenden - sie ist robust und benutzerfreundlich. 

```Kotlin
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

fun main() {
    val jetzt = LocalDateTime.now()
    println("Jetzt: $jetzt")

    val in5Tagen = jetzt.plus(5, ChronoUnit.DAYS)
    println("In 5 Tagen: $in5Tagen")
    
    val vor10Tagen = jetzt.minus(10, ChronoUnit.DAYS)
    println("Vor 10 Tagen: $vor10Tagen")
}
```

Die Ausgabe dieses Programms wird ähnlich sein wie:

```Kotlin
Jetzt: 2022-01-28T14:30:00.998
In 5 Tagen: 2022-02-02T14:30:00.998
Vor 10 Tagen: 2022-01-18T14:30:00.998
```

## Tiefer Einsteigen: 

Die `java.time` API, die wir hier nutzen, wurde mit JDK 8 eingeführt und ist eine große Verbesserung gegenüber älteren APIs wie `java.util.Date` oder `java.util.Calendar`. Sie ist einfacher zu bedienen und weniger fehleranfällig. 

Alternativen zur `java.time` API können Drittanbieterbibliotheken wie Joda-Time oder die Kotlin-native Klock Library sein. Allerdings werden diese oftmals als überflüssig angesehen, da `java.time` die meisten Anwendungsfälle bereits gut abdeckt.

Das Berechnen von zukünftigen und vergangenen Daten beruht auf der IETF-Standardspezifikation RFC 3339, die ein zeitzonen-unabhängiges Datenformat definiert.

## Siehe Auch:

Für weitere Informationen und Tutorials zur `java.time` API und zur Arbeit mit Daten in Java und Kotlin, sollten Sie folgende Quellen konsultieren:

1. [Official Java Documentation - java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
2. [Baeldung - Guide to java.time](https://www.baeldung.com/java-8-date-time-intro)
3. [Medium Article - Working with Date and Time in Kotlin](https://medium.com/@kashifo/working-with-date-and-time-in-kotlin-android-8743f1d00256) 

Happy coding!