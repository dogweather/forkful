---
title:                "Commandoregelargumenten lezen"
aliases:
- /nl/kotlin/reading-command-line-arguments.md
date:                  2024-01-28T22:05:34.623876-07:00
model:                 gpt-4-0125-preview
simple_title:         "Commandoregelargumenten lezen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Commandoregelargumenten lezen betekent data ophalen die aan je programma wordt doorgegeven wanneer het start. Programmeurs hebben dit nodig om gebruikers in staat te stellen het gedrag van een programma aan te passen zonder de code te wijzigen.

## Hoe te:

```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Hallo, ${args[0]}!")
    } else {
        println("Hallo, onbekende persoon!")
    }
}

// Voorbeelduitvoer als 'Kotlinista' als argument wordt doorgegeven:
// Hallo, Kotlinista!
```

In de bovenstaande code is `args` een array die de commandoregelargumenten bevat. De `main`-functie controleert of we er een hebben ontvangen en begroet dienovereenkomstig.

## Diepere Duik
Het concept van commandoregelargumenten is zo oud als de weg naar Rome; het is een deel van programmeren sinds het begin der tijden - of in ieder geval sinds de creatie van interactieve terminals. In de context van Kotlin, dat op de JVM draait, werken commandoregelargumenten vergelijkbaar met Java.

Andere talen bieden vergelijkbare middelen, zoals `argv` in Python of `$argc` en `$argv` in PHP. Kotlin's aanpak houdt het eenvoudig - de `main`-functie neemt gewoon een `Array<String>`.

Wat betreft implementatiedetails, onthoud dat de indices van arrays bij nul beginnen. `args[0]` is het eerste argument, `args[1]` is het tweede, enzovoort. Ook is het goed om in gedachten te houden dat als je een complexe app bouwt die commando's flexibeler moet ontleden, je misschien wilt kijken naar een gespecialiseerde bibliotheek zoals kotlinx-cli.

## Zie Ook
- [Kotlin's Officiële Documentatie over Command-Line Applicaties](https://kotlinlang.org/docs/command-line.html)
- [kotlinx-cli op GitHub](https://github.com/Kotlin/kotlinx-cli)
