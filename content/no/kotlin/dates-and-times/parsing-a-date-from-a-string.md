---
date: 2024-01-20 15:37:14.737512-07:00
description: "Dato-parsing fra en streng betyr \xE5 konvertere tekst til et `Date`-objekt.\
  \ Programmerere gj\xF8r dette for \xE5 h\xE5ndtere datoer og tider p\xE5 en standardisert,\u2026"
lastmod: '2024-03-13T22:44:40.760187-06:00'
model: unknown
summary: "Dato-parsing fra en streng betyr \xE5 konvertere tekst til et `Date`-objekt."
title: Tolke en dato fra en streng
weight: 30
---

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
