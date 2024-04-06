---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:22.396960-07:00
description: "Hoe te: Historisch gezien zijn datums een wirwar van problemen geweest\
  \ voor ontwikkelaars. Tijdzones, schrikkeljaren, zomertijd; ze zijn lastig. Kotlin\u2026"
lastmod: '2024-04-05T22:51:03.602824-06:00'
model: gpt-4-0125-preview
summary: Historisch gezien zijn datums een wirwar van problemen geweest voor ontwikkelaars.
title: Het huidige datum ophalen
weight: 29
---

## Hoe te:
```Kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("De datum van vandaag is: $today")
}
```

Voorbeelduitvoer:
```
De datum van vandaag is: 2023-04-05
```

## Diepere Duik
Historisch gezien zijn datums een wirwar van problemen geweest voor ontwikkelaars. Tijdzones, schrikkeljaren, zomertijd; ze zijn lastig. Kotlin vertrouwt vanaf Java 8 op de `java.time` API's van Java, wat het uitvoeren van datumoperaties dragelijker maakt.

`LocalDate.now()` is onze toevlucht voor de huidige datums. Geen tijd, geen zone – gewoon de datum. Tijd nodig? Dan is er `LocalTime`. Beide? `LocalDateTime`. En als tijdzones van belang zijn, gebruik je `ZonedDateTime`.

Alternatieven? Voor Java 8 heersten `java.util.Date` en `Calendar`. Niet geweldig, niet vreselijk, maar nu enigszins ouderwets en, eerlijk gezegd, minder intuïtief.

Onder de motorkap pikt `LocalDate.now()` de systeemklok op. Maar het is niet zomaar een klok – het is de UTC-klok, aangepast aan de standaardtijdzone van je systeem. Je kunt ermee rommelen, zeker – geef een andere `Clock` of `ZoneId` door als je graag op het randje leeft.

## Zie Ook
Kotlin documentatie over datums en tijden: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/)

Overzicht van Java 8 Datum/Tijd: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)

Wil je een volledige historicus worden? Check de evolutie van java.time: [https://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html](https://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html)
