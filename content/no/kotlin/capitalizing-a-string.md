---
title:                "Kotlin: Kapitalisering av en streng"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger, når du arbeider med tekststrenger i Kotlin, kan du ønske å kapitalisere en streng. Dette kan være nyttig hvis du for eksempel ønsker å gjøre en tittel mer visuelt tiltalende eller hvis du trenger å sikre deg at en brukers input er i riktig format. Heldigvis er dette enkelt å gjøre ved hjelp av innebygde funksjoner i Kotlin-språket!

## Slik gjør du det

For å kapitalisere en streng i Kotlin, kan du bruke funksjonen `capitalize()` som er tilgjengelig på enhver streng. La oss se på et eksempel:

```Kotlin
val navn = "per"
println(navn.capitalize()) // Output: Per
```

Som du kan se, vil funksjonen `capitalize()` gjøre om den første bokstaven i strengen til en stor bokstav. Men hva gjør du hvis du ønsker å kapitalisere alle ord i en setning? Da kan du bruke funksjonen `capitalizeWords()`. La oss ta en titt på et annet eksempel:

```Kotlin
val setning = "dette er en setning."
println(setning.capitalizeWords()) // Output: Dette Er En Setning.
```

Som du kan se, vil nå alle de første bokstavene i hvert ord bli kapitalisert. Det er også verdt å merke seg at `capitalize()` og `capitalizeWords()` begge har overbelastede funksjoner som lar deg angi hvilken språkkode som skal brukes for å håndtere forskjellige språk.

## Dypdykk

Nå som du vet hvordan du kapitaliserer en streng, la oss dykke litt dypere inn i hvordan denne funksjonen faktisk fungerer. Når du kaller `capitalize()` på en streng, konverterer Kotlin den internt til en `StringBuilder` og bruker deretter funksjonen `setCharAt()` for å endre den første bokstaven til en stor bokstav.

I tillegg er det viktig å merke seg at både `capitalize()` og `capitalizeWords()` returnerer en ny streng i stedet for å endre den opprinnelige strengen. Dette er fordi strenger er uforanderlige (immutable) i Kotlin-språket.

## Se Også

Hvis du ønsker å lære mer om hvordan du arbeider med strenger i Kotlin, kan du sjekke ut disse ressursene:

- [Offisiell Kotlin-dokumentasjon om strenger](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Kotlin Strings Cheat Sheet](https://blog.kotlin-academy.com/kotlin-string-cheat-sheet-22d766f9868d)
- [Kotlin Strings Tutorial](https://www.tutorialkart.com/kotlin/kotlin-strings/)

Lykke til med kapitaliseringen av strenger i Kotlin!