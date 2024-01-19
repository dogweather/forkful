---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å finne lengden på en streng betyr å telle antall tegn den inneholder. Dette er viktig for mange programmeringsbehov, som validering av inndatat og tekstbehandling.

## Hvordan gjøre:

Kotlin tilbyr en innebygd metode, kalt `length`, for å finne lengden på en streng. Her er et enkelt eksempel:

```Kotlin
fun main() {
    val streng = "Kotlin er gøy"
    println("Lengden av strengen er: ${streng.length}")
}
```

Da vil utskriften være:

```Kotlin
Lengden av strengen er: 13
```

## Dyp Dykk

Historie: Måten å finne lengden på en streng på, har ikke endret seg mye med tiden. De fleste språk, inkludert Kotlin, tilbyr en innebygd metode for dette.

Alternativer: Du kan også bruke `count()` funksjonen i Kotlin for å få lengden på en streng, men `length` brukes oftere fordi den er raskere.

Implementering: Under hetta, `length` funksjonen i Kotlin bruker den innebygde `length`-egenskapen til Java String som returnerer antall 16-biter Unicode-tegn i strengen.

## Se også:

- Kotlin Dokumentasjon: [Basic types - String](https://kotlinlang.org/docs/basic-types.html#strings)
- Kotlin Koans: [String Templates](https://kotlinlang.org/docs/koans.html#string-templates)