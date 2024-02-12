---
title:                "Het huidige datum ophalen"
aliases:
- /nl/kotlin/getting-the-current-date.md
date:                  2024-01-28T22:01:22.396960-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het huidige datum ophalen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
We halen de huidige datum op om de gegevens van vandaag te kennen. Het is cruciaal voor tal van functies – denk aan logs, proefversies, evenementen. Wat je ook bedenkt, datums zijn vaak van groot belang.

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
