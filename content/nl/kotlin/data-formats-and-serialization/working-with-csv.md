---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:08.224727-07:00
description: "Hoe: Om met CSV in Kotlin te werken, kun je de kernbibliotheek gebruiken\
  \ of externe bibliotheken zoals Kotlinx.serialization of Apache Commons CSV. Hier\u2026"
lastmod: '2024-03-13T22:44:50.790120-06:00'
model: gpt-4-0125-preview
summary: Om met CSV in Kotlin te werken, kun je de kernbibliotheek gebruiken of externe
  bibliotheken zoals Kotlinx.serialization of Apache Commons CSV.
title: Werken met CSV
weight: 37
---

## Hoe:
Om met CSV in Kotlin te werken, kun je de kernbibliotheek gebruiken of externe bibliotheken zoals Kotlinx.serialization of Apache Commons CSV. Hier laat ik je de basis I/O zien zonder externe bibliotheken.

```kotlin
import java.io.File

fun main() {
    // Schrijven naar CSV
    val outputFile = File("data.csv")
    outputFile.printWriter().use { out ->
        out.println("id,nam,leeftijd")
        out.println("1,John Doe,30")
        out.println("2,Jane Smith,25")
    }

    // Lezen van CSV
    File("data.csv").forEachLine { line ->
        val (id, naam, leeftijd) = line.split(',')
        println("ID: $id, Naam: $naam, Leeftijd: $leeftijd")
    }
}
```

Output:
```text
ID: 1, Naam: John Doe, Leeftijd: 30
ID: 2, Naam: Jane Smith, Leeftijd: 25
```

## Diepere Duik
De wortels van CSV gaan terug naar de vroege dagen van het computer tijdperk toen geheugen beperkt was en data uitwisselingsformaten eenvoudig moesten zijn. Hoewel alternatieven zoals JSON en XML zijn opgekomen, blijft CSV populair vanwege het gebruiksgemak, de compatibiliteit en omdat het leesbaar is voor mensen.

De juiste omgang met CSV kan complexer zijn vanwege randgevallen (zoals komma's in gegevens, velden over meerdere regels, enz.). Bibliotheken zoals Apache Commons CSV en Kotlinx.serialization gaan om met deze gevallen en bieden extra functionaliteit.

## Zie Ook
- [RFC 4180](https://tools.ietf.org/html/rfc4180): Het gemeenschappelijke formaat en MIME-type voor CSV-bestanden.
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/): Een Java-bibliotheek voor het omgaan met CSV-bestanden die in Kotlin kan worden gebruikt.
- [Kotlinx.serialization CSV](https://github.com/Kotlin/kotlinx.serialization): Een Kotlin-bibliotheek die serialisatie naar en van CSV-formaat vereenvoudigt.
