---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:37.222256-07:00
description: 'Hoe te: Hier is een voorproefje van het debuggen in Kotlin met IntelliJ
  IDEA - de Sherlock Holmes van IDE''s.'
lastmod: '2024-03-13T22:44:50.772386-06:00'
model: gpt-4-0125-preview
summary: Hier is een voorproefje van het debuggen in Kotlin met IntelliJ IDEA - de
  Sherlock Holmes van IDE's.
title: Een debugger gebruiken
weight: 35
---

## Hoe te:
Hier is een voorproefje van het debuggen in Kotlin met IntelliJ IDEA - de Sherlock Holmes van IDE's:

```kotlin
fun main() {
    val mysterieNummer = 42
    var gok = 0

    while (gok != mysterieNummer) {
        println("Raad het nummer: ")
        gok = readLine()?.toIntOrNull() ?: continue // Negeer slechte input

        // Zet hier een breakpoint om 'gok' in actie te zien
        if (gok < mysterieNummer) {
            println("Te laag!")
        } else if (gok > mysterieNummer) {
            println("Te hoog!")
        }
    }

    println("Je hebt het geraden! Het mysterienummer was $mysterieNummer")
}
```

Debuggeruitvoer:
```
Raad het nummer:
10
Te laag!
Raad het nummer:
50
Te hoog!
Raad het nummer:
42
Je hebt het geraden! Het mysterienummer was 42
```

## Diepere duik
Debuggers zijn al in het spel sinds de jaren '50. Destijds waren ze vrij primitief en kon debuggen meer over hardware dan software gaan. Tegenwoordig stelt een debugger zoals die in IntelliJ IDEA ons in staat om breakpoints te zetten, regel voor regel door de code te stappen en op ons gemak de staat van variabelen te inspecteren.

Hoewel de debugger van IntelliJ uitermate handig is voor Kotlin, is het niet de enige vis in de zee. Er is een reeks alternatieven zoals Logcat voor Android-ontwikkeling of opdrachtregelhulpmiddelen zoals jdb voor de minimalisten. De magie achter de schermen gaat vooral over JVM Tool Interface (JVMTI), dat debuggers in staat stelt om te interageren met de Java Virtual Machine en Kotlin-ontwikkelaars in de lus te houden.

## Zie ook
- IntelliJ IDEA Debugger documentatie: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
