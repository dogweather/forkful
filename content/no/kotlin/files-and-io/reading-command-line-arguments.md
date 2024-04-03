---
date: 2024-01-20 17:56:14.191718-07:00
description: "Hvordan: Her er et eksempel p\xE5 hvordan \xE5 lese kommandolinjeargumenter\
  \ i Kotlin."
lastmod: '2024-03-13T22:44:40.766740-06:00'
model: gpt-4-1106-preview
summary: "Her er et eksempel p\xE5 hvordan \xE5 lese kommandolinjeargumenter i Kotlin."
title: Lese kommandolinjeargumenter
weight: 23
---

## Hvordan:
Her er et eksempel på hvordan å lese kommandolinjeargumenter i Kotlin:

```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Hei, ${args[0]}!")
    } else {
        println("Hei, ukjent bruker!")
    }
}
```
Kjøre programmet: `kotlinc helloworld.kt -include-runtime -d helloworld.jar && java -jar helloworld.jar Ola`
Forventet output: `Hei, Ola!`

## Dypdykk
Lesing av kommandolinjeargumenter er noe som stammer fra tidlige dagers programmering. Alternativer til argumenter på kommandolinjen inkluderer konfigurasjonsfiler, miljøvariabler eller interaktive brukergrensesnitt. Det er vanlig å bruke et bibliotek som "kotlinx-cli" for mer avansert håndtering av argumenter. I reine Kotlin er `Array<String>` gitt til `main`-funksjonen standardmetoden for å akseptere disse argumentene.

## Se Også
- [Kotlin Documentation: Command-line Interface](https://kotlinlang.org/docs/command-line.html)
- [GitHub: kotlinx-cli](https://github.com/Kotlin/kotlinx-cli)
- [Stack Overflow: How to parse command line arguments in Kotlin?](https://stackoverflow.com/questions/41728426/how-to-parse-command-line-arguments-in-kotlin)
