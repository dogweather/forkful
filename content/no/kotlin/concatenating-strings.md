---
title:    "Kotlin: Sammenslåing av tekststrenger"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor 

I dagens digitale verden er det viktigere enn noensinne å kunne behandle tekst og data effektivt. En måte å gjøre dette på er gjennom å lagre og kombinere tekststrenger (også kjent som concatenation) i programmering. Dette kan være nyttig for språkbehandling, skrive ut informasjon og mye mer.

## Slik gjør du det

For å konkatenere strenger i Kotlin kan du bruke operator "+" mellom to strenger. Dette vil kombinere dem og lagre dem som én streng. For eksempel:

```Kotlin

val navn = "Sara"

val velkomst = "Hei " + navn

println(velkomst) // Resultat: "Hei Sara"
```

Du kan også bruke funksjonen "format()" for å bytte ut variabler inne i en streng. Her er et eksempel:

```Kotlin
val vekt = 62

val setning = "Jeg veier %d kg".format(vekt)

println(setning) // Resultat: "Jeg veier 62 kg"
```

## Dykk dypere 

I tillegg til å kombinere strenger, kan du også bruke metoder for å endre og manipulere tekst. For eksempel kan du bruke metodene "toUpperCase()" eller "toLowerCase()" for å konvertere tekst til store eller små bokstaver. Du kan også bruke "replace(oldValue, newValue)" for å bytte ut deler av tekst med en annen.

En annen viktig ting å merke seg er at i Kotlin, siden tekst er uforanderlig, lages en ny streng hver gang en metode blir brukt. Dette kan føre til unødvendig arbeid og påvirke ytelsen til programmet ditt.

## Se også 

- [Kotlin offisiell dokumentasjon] (https://kotlinlang.org/docs/strings.html) 
- [Java Tutorial Network] (https://www.javatutorialnetwork.com/kotlin/string-tutorials) 
- [W3Schools] (https://www.w3schools.com