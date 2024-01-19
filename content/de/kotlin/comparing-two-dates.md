---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Vergleichen zweier Daten ist ein einfacher Weg festzustellen, welches Datum früher oder später ist. Entwickler nutzen diese Methode, um Zeitabläufe in Anwendungen zu organisieren oder zu steuern.

## Anleitung: 

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2020, 1, 1)
    val date2 = LocalDate.of(2021, 1, 1)

    when {
        date1.isBefore(date2) -> println("date1 ist vor date2")
        date1.isAfter(date2) -> println("date1 ist nach date2")
        else -> println("date1 und date2 sind gleich")
    }
}
```

Das obige Code-Beispiel wird "date1 ist vor date2" ausgeben, da das Datum von `date1` der 1. Januar 2020 und das von `date2` der 1. Januar 2021 ist.

## Tiefergehende Informationen: 

Historisch wurde das Vergleichen von Daten in Kotlin nur über komplexe Umwege und eigene Algorithmen ermöglicht. Mit der aufkommenden `java.time`-Bibliothek im JDK 8, wurden diese Herausforderungen jedoch leichter. Alternativen zum direkten Vergleichen zweier Daten können das Extrahieren und Vergleichen einzelner Datumskomponenten (Tag, Monat, Jahr) sein. Allerdings führen solche Vorgehensweisen oft zu längeren und fehleranfälligeren Code.

## Weiterführende Themen:

- Kotlin Dokumentation zu LocalDate: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/
- Java Time API: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Informative Beispieldemonstration über LocalDate in Kotlin: https://www.baeldung.com/kotlin-dates