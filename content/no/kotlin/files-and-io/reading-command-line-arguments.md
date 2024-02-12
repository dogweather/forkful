---
title:                "Lese kommandolinjeargumenter"
aliases:
- /no/kotlin/reading-command-line-arguments/
date:                  2024-01-20T17:56:14.191718-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kommandolinjeargumenter lar deg sende informasjon til programmet ditt ved oppstart, slik at oppførselen kan tilpasses på fly. Programmerere gjør dette for å lage fleksible applikasjoner som kan håndteres dynamisk uten endring av koden.

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
