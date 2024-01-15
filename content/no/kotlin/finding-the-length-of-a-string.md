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

## Hvorfor

Å finne lengden til en streng er en svært vanlig oppgave som en programmerer støter på. Det kan være nyttig å vite hvor mange tegn en streng inneholder for å kunne behandle den på riktig måte.

## Hvordan

Å finne lengden til en streng i Kotlin er enkelt og intuitivt. Du trenger bare å kalle på `length` metoden for å få tilbake antall tegn i en streng. Se eksempelet nedenfor:

```Kotlin
val navn = "Ole"
println(navn.length)
```

Output:
`3`

Du kan også bruke `count()` metoden for å telle antall tegn i en streng. Det følgende eksempelet viser hvordan:

```Kotlin
val setning = "Dette er en setning."
println(setning.count())
```

Output:
`21`

## Dypdykk

I Kotlin er det ingen forskjell mellom `length` og `count()` metodene. Begge gir samme resultat, og bruker de samme underliggende mekanismene for å finne lengden til en streng. Det er viktig å være klar over at både whitespace og spesialtegn telle som et tegn når du bruker disse metodene. Du kan også bruke `size()` metoden på en `String` variabel for å finne lengden.

## Se Også

- [Kotlin Offisiell Dokumentasjon](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin Strings Tutorial](https://www.programiz.com/kotlin-programming/strings)