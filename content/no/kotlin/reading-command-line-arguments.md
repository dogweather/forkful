---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Kotlin: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Lesing av kommandolinjeargumenter er en måte for programmerere å få informasjon fra brukeren mens programmet kjører. Dette er nyttig for å tilpasse utførelsen av programmet eller for å ta brukerens valg og handlinger i betraktning.

## Hvordan:

```Kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) { // Sjekker om det er argumenter som er gitt
        println("Følgende argumenter ble gitt:")
        for (arg in args) {
            println(arg) // Skriver ut hvert argument
        }
    } else {
        println("Ingen argumenter ble gitt.")
    }
}

// Eksempel kjøring:
// Inndata: kotlin article.kt hello world
// Output: Følgende argumenter ble gitt:
// hello
// world
```

## Dypdykk:

Historisk sett måtte programmer ta imot alle inndata fra terminalen ved hjelp av kommandolinjeargumenter. Alternativer til å lese kommandolinjeargumenter inkluderer å lese inndata fra en fil eller å bruke et grafisk brukergrensesnitt. I Kotlin, bruker vi `main`-funksjonen og dens `args`-parameter til å lese kommandolinjeargumenter.

## Se også:

[Offisiell Kotlin Dokumentasjon](https://kotlinlang.org/docs/tutorials/command-line.html)

[Kotlin Dokumentasjon om funksjoner](https://kotlinlang.org/docs/reference/functions.html)