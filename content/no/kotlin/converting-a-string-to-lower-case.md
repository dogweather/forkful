---
title:                "Kotlin: Konvertere en streng til små bokstaver"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Når du jobber med tekster i Kotlin, kan du komme over situasjoner der du trenger å endre teksten til små bokstaver. Dette er nyttig for å sammenligne ulike tekststrenger eller for å sørge for at din input er i riktig form. Ved å konvertere en streng til små bokstaver, kan du gjøre søket mer nøyaktig og unngå feil i koden din. Derfor er det viktig å vite hvordan du kan konvertere en streng til små bokstaver i Kotlin.

## Hvordan
For å konvertere en streng til små bokstaver i Kotlin, kan du bruke funksjonen `toLowerCase ()`. Denne funksjonen vil endre alle bokstavene i strengen til små bokstaver og returnere den endrede strengen. Her er et eksempel på hvordan du kan bruke denne funksjonen i kode:

```Kotlin
val tekst = "HELLO KOTLIN"
val endretTekst = tekst.toLowerCase()
println(endretTekst)
```

Output: `hello kotlin`

Du kan også bruke denne funksjonen direkte på en streng når du sammenligner eller bruker den:

```Kotlin
if (input.toLowerCase() == "ja") {
    println("Korrekt svar!")
}
```

## Deep Dive
Når du bruker `toLowerCase ()` i Kotlin, kan det hende du lurer på hva som skjer under overflaten. Denne funksjonen er en del av standardbiblioteket i Kotlin og er implementert ved hjelp av standard Unicode-spesifikasjoner. Dette betyr at funksjonen vil fungere for alle språk og karaktersett som støttes av Unicode.

En viktig ting å merke seg er at `toLowerCase ()` funksjonen vil endre alle bokstaver, uavhengig av språk og alfabet. Dette betyr at du må være nøye med hvilken inngangsverdi du bruker, slik at den ikke endrer meningen av teksten din.

## Se Også
- [Kotlin strings](https://kotlinlang.org/docs/strings.html)
- [Unicode Character Database](https://www.unicode.org/ucd/)