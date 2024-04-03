---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:37.824478-07:00
description: "Hoe te: Kotlin behandelt datums en tijden met de `java.time` bibliotheek.\
  \ Om dagen toe te voegen of af te trekken, gebruik je `plusDays()` of\u2026"
lastmod: '2024-03-13T22:44:50.781311-06:00'
model: gpt-4-0125-preview
summary: Kotlin behandelt datums en tijden met de `java.time` bibliotheek.
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Hoe te:
Kotlin behandelt datums en tijden met de `java.time` bibliotheek. Om dagen toe te voegen of af te trekken, gebruik je `plusDays()` of `minusDays()`. Hier is de korte uitleg:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    val tienDagenLater = today.plusDays(10)
    val tienDagenVoor = today.minusDays(10)
    
    println("Vandaag: $today")
    println("Over tien dagen: $tienDagenLater")
    println("Tien dagen geleden: $tienDagenVoor")
}
```

Voorbeelduitvoer:

```
Vandaag: 2023-03-15
Over tien dagen: 2023-03-25
Tien dagen geleden: 2023-03-05
```

Naast dagen, kun je ook spelen met maanden en jaren (`plusMonths()`, `minusMonths()`, `plusYears()`, `minusYears()`).

## Diepgaande Duik
Datums berekenen is niet nieuw. Sinds Java 8 is het `java.time` pakket de go-to voor datum-tijd rekenwerk - veel beter dan de oude `Calendar` of `Date`, die onhandig en niet thread-safe waren.

`java.time` gebruikt onveranderlijke objecten, zodat je nare bugs vermijdt door per ongeluk je datums te wijzigen. Objecten zoals `LocalDate`, `LocalTime`, `LocalDateTime` en `ZonedDateTime` helpen je verschillende aspecten van tijd precies weer te geven.

Alternatieven? Natuurlijk. Voor `java.time` was Joda-Time de wapenkeuze. Sommige oudere systemen gebruiken het nog steeds. En in de Android-wereld backport de ThreeTenABP-bibliotheek `java.time` functies voor compatibiliteit met Java 6 & 7 omstandigheden.

De `java.time` API is ook ontworpen om rekening te houden met tijdzones, dankzij klassen zoals `ZonedDateTime`. Dus wanneer je met datums schuift, kun je de chronologie van de draaiing van de aarde respecteren.

## Zie Ook
- De officiële `java.time` documentatie: [Java SE Datum Tijd](https://docs.oracle.com/javase/tutorial/datetime/)
- Voor de Android-ontwikkelaars, details over de `ThreeTenABP` bibliotheek: [ThreeTenABP op GitHub](https://github.com/JakeWharton/ThreeTenABP)
- Een diepgaande handleiding, als je meer ins en outs over datum en tijd wilt: [Datum en Tijd in Java](https://www.baeldung.com/java-8-date-time-intro)
