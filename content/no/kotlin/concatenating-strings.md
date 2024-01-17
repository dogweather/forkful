---
title:                "Sammenslåing av strenger"
html_title:           "Kotlin: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sette sammen flere tekststrenger, eller "concatenating strings" på engelsk, er en vanlig operasjon i programmering. Det innebærer å kombinere to eller flere tekststrenger for å danne en enkelt streng. Dette er nyttig når du vil lage en lengre tekst fra mindre deler, for eksempel når du skriver ut en komplett setning eller en beskrivelse.

Programmerere bruker ofte concatenating strings for å programmere dynamiske og fleksible løsninger. Det lar dem sette sammen tekst på forskjellige måter basert på brukerens input eller andre variabler. Dette gjør det enklere å lage mer allsidige programmer.

## Hvordan:
Du kan enkelt concatenate strings i Kotlin ved å bruke "+" -operatøren eller "plus" -funksjonen. Denne operatøren/funksjonen kombinerer to eller flere tekststrenger og returnerer en ny streng som inneholder alle de opprinnelige strengene.

```Kotlin
val fornavn = "Kari"
val etternavn = "Nordmann"
val fulltNavn = fornavn + " " + etternavn
println(fulltNavn)

// Output: Kari Nordmann
```

Du kan også bruke plus-operatør med en variabel og en tekststreng:

```Kotlin
val alder = 25
val beskrivelse = "Jeg er " + alder.toString() + " år gammel"
println(beskrivelse)

// Output: Jeg er 25 år gammel
```

## Dypdykk:
Å sette sammen tekststrenger er en viktig del av moderne programmering og har vært en del av programmeringsspråk siden begynnelsen. Først ble det utført ved å bruke spesielle funksjoner eller operatører for å kombinere tegn, men med fremveksten av objektorientert programmering ble det enklere å manipulere tekststrenger som variabler.

I Kotlin er det også alternativer for å concatenate strings, som "plusAssign" -operatøren som kan brukes for å legge til en tekststreng til slutten av en eksisterende variabel uten å måtte opprette en ny variabel.

## Se også:
- [Kotlin Docs for Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Java String Concatenation History](https://en.wikipedia.org/wiki/Concatenation#History)