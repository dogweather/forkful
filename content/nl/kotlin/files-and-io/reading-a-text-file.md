---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:14.479120-07:00
description: "Een tekstbestand lezen betekent het binnenhalen van data uit een bestand\
  \ in je programma, typisch regel voor regel. Programmeurs doen dit om data die\u2026"
lastmod: '2024-03-13T22:44:50.785206-06:00'
model: gpt-4-0125-preview
summary: Een tekstbestand lezen betekent het binnenhalen van data uit een bestand
  in je programma, typisch regel voor regel.
title: Een tekstbestand lezen
weight: 22
---

## Hoe:
In Kotlin kun je makkelijk een tekstbestand lezen met de functie `readLines()` of het `useLines` blok.

```Kotlin
import java.io.File

fun main() {
    // Lees alle regels in één keer
    val lines = File("voorbeeld.txt").readLines()
    lines.forEach { regel ->
        println(regel)
    }

    // Efficiënter voor grote bestanden
    File("voorbeeld.txt").useLines { regels ->
        regels.forEach { regel ->
            println(regel)
        }
    }
}
```

Voorbeelduitvoer (ervan uitgaande dat `voorbeeld.txt` twee regels bevat met "Hallo" en "Wereld"):

```
Hallo
Wereld
```

## Diepgaand
Historisch gezien kon het lezen van bestanden in Java omslachtig en langdradig zijn. Met Kotlin biedt de standaardbibliotheek handige uitbreidingen om het lezen van bestanden eenvoudiger te maken.

Er zijn alternatieven voor het lezen van bestanden in Kotlin:
1. `readText()` leest de gehele bestandsinhoud in één `String`.
2. `bufferedReader()` biedt een `BufferedReader` die je in staat stelt om complexere gevallen te behandelen, zoals het lezen van enorme bestanden zonder te veel geheugen te verbruiken.

Wat de implementatie betreft, wanneer je `useLines` gebruikt, zorgt het ervoor dat het bestand wordt gesloten na uitvoering, waardoor potentiële geheugenlekken worden voorkomen. Het is een functionele aanpak die in Kotlin wordt aangemoedigd voor effectief beheer van bronnen.

## Zie Ook
- Kotlin-documentatie over het lezen van bestanden: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- `BufferedReader` documentatie voor meer complexe gevallen: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/)
