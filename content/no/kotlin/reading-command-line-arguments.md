---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Kommandolinjeargumenter er data som sendes inn i et program når det kjøres. Det gi en fleksibel måte for brukere å påvirke en applikasjon, uten dets koden.

## Hvordan gjør man det:

Her er et enkelt eksempel på hvordan vi kan lese kommandolinjeargumenter i Kotlin.

```
fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
}
```

Når vi kjører programmet vårt med `kotlin MainKt arg1 arg2 arg3`, får vi følgende utskrift:

```
arg1
arg2
arg3
```

Dette eksemplet tar imot en liste av strenger (`args: Array<String>`) som argumenter til main-funksjonen. Den går igjennom hver streng i listen og skriver ut til konsollen.

## Dypdykk

Historisk sett, begynte ideen om å gi inpit til et program ved kommandolinjen med UNIX operativsystemer. Noen alternative måter å godta input på inkluderer interaktiv brukerinput, lesing av filer og bruk av en GUI.

Når det gjelder implementeringsdetaljer, så vil telling og håndtering av kommandolinjeargumenter i Kotlin kraftig avhenge av hva du prøver å oppnå med applikasjonen din. For mer komplekse scenarier kan det være hensiktsmessig å bruke et bibliotek for kommandolinjevalg som JCommander eller Kotlin-Argparser.

## Se Også

Hvis du vil lese mer om dette emnet, sjekk ut disse lenkene:
- Offisiell Kotlin Dokumentasjon: https://kotlinlang.org/docs/keyword-reference.html
- Introduksjon til kommandolinjeargumenter: https://en.wikipedia.org/wiki/Command-line_interface#Arguments
- JCommander: http://jcommander.org/
- Kotlin-Argparser: https://github.com/xenomachina/kotlin-argparser