---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:37:14.737512-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Dato-parsing fra en streng betyr å konvertere tekst til et `Date`-objekt. Programmerere gjør dette for å håndtere datoer og tider på en standardisert, manipulerbar måte i apps og systemer.

## How to:
I Kotlin parser vi datoer ved hjelp av `java.time`-biblioteket. Her er et eksempel:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-04-02"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val date = LocalDate.parse(dateString, formatter)
    
    println(date) // Skriver ut: 2023-04-02
}
```

Og med et annet format:

```Kotlin
import java.time.LocalDateTime

fun main() {
    val dateTimeString = "02.04.2023 14:00"
    val formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm")
    val dateTime = LocalDateTime.parse(dateTimeString, formatter)

    println(dateTime) // Skriver ut: 2023-04-02T14:00
}
```

## Deep Dive
Før `java.time` kom i Java 8, brukte programmerere `SimpleDateFormat` fra `java.util` som var mindre robust og ikke trådsikker. `java.time`-biblioteket introduserte en mer intuitiv API og faste datamodeller. 

Som alternativ til innebygd parsering, kan du bruke tredjepartsbiblioteker som Joda-Time, men dette er ofte ikke nødvendig siden `java.time` er så kraftig. Ved implementering er det viktig å forstå datoformatet du jobber med. Mønsteret i `DateTimeFormatter` må matche strengen eksakt, ellers kastes en `DateTimeParseException`.

## See Also
- [Official Java Time Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [DateTimeFormatter documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
