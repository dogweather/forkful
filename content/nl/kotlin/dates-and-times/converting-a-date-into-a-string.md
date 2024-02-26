---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:28.891767-07:00
description: "Een datum naar een string converteren betekent dat je een specifiek\
  \ moment representeert in een voor mensen leesbaar formaat. Programmeurs doen dit\
  \ om\u2026"
lastmod: '2024-02-25T18:49:48.118727-07:00'
model: gpt-4-0125-preview
summary: "Een datum naar een string converteren betekent dat je een specifiek moment\
  \ representeert in een voor mensen leesbaar formaat. Programmeurs doen dit om\u2026"
title: Een datum converteren naar een string
---

{{< edit_this_page >}}

## Wat & Waarom?
Een datum naar een string converteren betekent dat je een specifiek moment representeert in een voor mensen leesbaar formaat. Programmeurs doen dit om datums aan gebruikers te tonen of om ze te serialiseren voor opslag en gegevensoverdracht.

## Hoe te:
In Kotlin kun je een `Date` naar een `String` converteren met behulp van de `SimpleDateFormat` klasse. Laten we wat code schrijven:

```kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val date = Date() // Maak een Date object voor de huidige tijd
    val format = SimpleDateFormat("yyyy-MM-dd HH:mm:ss") // Definieer het datumformaat
    val dateString = format.format(date) // Converteer Date naar String
    println(dateString) // Output de datumstring
}
```

Een voorbeelduitvoer kan er zo uitzien:

```
2023-03-25 14:45:32
```

## Diepere duik
Voordat `java.time` op de scene kwam, was `SimpleDateFormat` dé manier voor datum-string transformaties in Java en, bij overerving, in Kotlin. Ja, Kotlin draait op de Java Virtual Machine en werkt comfortabel met Java bibliotheken.

Met Java 8 kwam echter `java.time` in beeld, met de `DateTimeFormatter` die een veel meer verfijnde API bracht. Dit was een game-changer, die veiligere, onveranderlijke en thread-safe datum-tijd manipulatie bood. De native ondersteuning van Kotlin hiervoor is naadloos:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val currentDate = LocalDateTime.now() // Haal de huidige datum en tijd op
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formattedDate = currentDate.format(formatter)
    println(formattedDate)
}
```

Alternatieven? Zeker. Voor niet-standaard vereisten of het jongleren tussen meerdere datum bibliotheken, waren opties van derden zoals Joda-Time vroeger de gouden standaard. Tegenwoordig dekt `java.time` de meeste bases.

Wat implementatiedetails betreft, `SimpleDateFormat` is niet thread-safe, wat betekent dat het kan struikelen wanneer het wordt gebruikt in gelijktijdige settings. `DateTimeFormatter` heeft dat probleem niet. Maak één keer aan, gebruik voor altijd - of in ieder geval gedurende je applicatie zonder veel zorgen.

## Zie ook
- `DateTimeFormatter` JavaDoc voor al je patroonbehoeften: [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- Als je nostalgisch bent of voorbeelden nodig hebt voor legacy-systemen, hier is de inside informatie over `SimpleDateFormat`: [SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
