---
title:                "Å finne lengden på en streng"
html_title:           "Kotlin: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å finne lengden på en streng betyr å telle antall tegn i en gitt streng. Dette er en vanlig oppgave for programmører, da det ofte er nødvendig å vite lengden på en streng for å kunne behandle den riktig.

## Hvordan:
Kotlin gjør det enkelt å finne lengden på en streng ved å bruke innebygde metoder. Her er et eksempel på hvordan du kan finne lengden på en streng:

```Kotlin
fun main() {
    val str = "Hei, verden!"
    println("Lengden på strengen er: ${str.length}")
}
```

Output:
```
Lengden på strengen er: 12
```

## Dypdykk:
Å finne lengden på en streng er en enkel oppgave i de fleste programmeringsspråk, men det har ikke alltid vært tilfelle. I tidligere språk var det vanlig å bruke en egen funksjon for å telle lengden på en streng, men med utviklingen av moderne språk som Kotlin, har dette blitt enklere og mer effektivt.

Et alternativ til å bruke ```.length``` metoden i Kotlin er å bruke ```.count``` metoden, som gir samme resultat. Dette er nyttig å vite hvis man jobber med eldre kode eller må bruke et annet språk.

## Se også:
Kotlin Dokumentasjon - Strings: https://kotlinlang.org/docs/reference/basic-types.html#strings