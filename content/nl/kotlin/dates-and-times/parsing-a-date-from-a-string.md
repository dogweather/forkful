---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:24.308098-07:00
description: "Het ontleden van een datum betekent het omzetten van een datum in tekstformaat\
  \ naar een datumobject dat een programma kan begrijpen en manipuleren. Het is\u2026"
lastmod: '2024-02-25T18:49:48.116713-07:00'
model: gpt-4-0125-preview
summary: "Het ontleden van een datum betekent het omzetten van een datum in tekstformaat\
  \ naar een datumobject dat een programma kan begrijpen en manipuleren. Het is\u2026"
title: Een datum uit een string parsen
---

{{< edit_this_page >}}

## Wat & Waarom?
Het ontleden van een datum betekent het omzetten van een datum in tekstformaat naar een datumobject dat een programma kan begrijpen en manipuleren. Het is cruciaal voor het lezen van gegevens uit verschillende bronnen zoals gebruikersinvoer of bestanden, waardoor programma's datums en tijden consistent kunnen verwerken en hanteren.

## Hoe te:
Met Kotlin kun je datums ontleden met behulp van de `LocalDateTime` klasse uit het `java.time` pakket. Laten we een tekenreeks naar een datum ontleden.

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val datumTekenreeks = "2023-04-01T15:30:00"
    val formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    val ontledenDatum = LocalDateTime.parse(datumTekenreeks, formatter)
    
    println(ontledenDatum)  // Voorbeelduitvoer: 2023-04-01T15:30
}
```

## Diepduiken
Kotlin heeft geen eigen bibliotheek voor datum en tijd. In plaats daarvan vertrouwt het op de `java.time` API die is geïntroduceerd in Java 8, die oudere, minder intuïtieve datumklassen zoals `java.util.Date` heeft vervangen.

Een groot voordeel van `java.time` is dat het onveranderlijkheid en draadveiligheid naar datum-tijdbewerkingen heeft gebracht. Voor Java 8 zou je vaak naar externe bibliotheken zoals Joda-Time wenden voor robuuste datumafhandeling.

Bij het ontleden van datums moet je de datumtekenreeks afstemmen op het juiste formaat. Anders krijg je te maken met een `DateTimeParseException`. Kotlin's aanpak is afgeleid van de ISO 8601-standaard, maar je kunt met `DateTimeFormatter` aangepaste formaten creëren voor verschillende tekenreeks patronen.

Alternatieven voor `LocalDateTime` zijn onder meer `ZonedDateTime` voor ondersteuning van tijdzones of `LocalDate` en `LocalTime` voor het afzonderlijk ontleden van datums en tijden. Kotlin's flexibiliteit met de `java.time` API zorgt ervoor dat je jouw datumontleding kunt aanpassen aan de behoeften van het programma.

## Zie ook
- De officiële Java `DateTimeFormatter` documentatie: [https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- Kotlin Documentatie over Java Interoperabiliteit: [https://kotlinlang.org/docs/java-interop.html](https://kotlinlang.org/docs/java-interop.html)
- ISO 8601 Datum- en Tijdformaten: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
