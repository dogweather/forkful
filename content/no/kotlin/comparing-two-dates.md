---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sammenligne to datoer, innebærer å evaluere om en dato er tidligere, senere enn eller samme som en annen dato. Dette er viktig i programmering for f.eks tidsbestemt funksjonalitet, gyldighetskontroll, eller sortering.

## Hvordan til:
Her er et eksempel på hvordan man kan sammenligne to datoer i Kotlin.

```Kotlin
import java.time.LocalDate

fun main() {
    val idag = LocalDate.now()
    val fremtid = LocalDate.of(2025, 12, 31)

    when {
        idag.isBefore(fremtid) -> println("$idag er før $fremtid")
        idag.isAfter(fremtid) -> println("$idag er etter $fremtid")
        else -> println("Datoene er like")
    }
}
```
Prøv å kjøre koden, og du vil se at programmet vil printe ["dagens dato" er før 2025-12-31].

## Dypdykk
Kotlin bruker Java's LocalDateTime, LocalDate, og LocalTime klasser fra Java 8 til dato- og tidsfunksjoner. Før Java 8, måtte programmerere benytte `java.util.Date` og `java.util.Calendar`, som hadde kompliserte APIer og var vanskelige å bruke.

Et alternativ til Kotlin's innebygde støtte for dato- og tidsfunksjoner er Joda-Time biblioteket. Joda-Time har et mer omfattende API, og tillater mer kompliserte operasjoner ikke tilgjengelige i Kotlin.

Hvis du ønsker å sammenligne datoer basert på tidssoner, kan du benytte `ZonedDateTime`.

## Se også
For flere detaljer om hvordan Kotlin håndterer datoer, sjekk følgende lenker:

- Java 8 Date-Time API: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Kotlin dok: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date-time/
- Joda-Time: http://www.joda.org/joda-time/