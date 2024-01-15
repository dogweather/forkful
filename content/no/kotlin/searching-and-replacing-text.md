---
title:                "Søking og utskifting av tekst"
html_title:           "Kotlin: Søking og utskifting av tekst"
simple_title:         "Søking og utskifting av tekst"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan man trenge å erstatte tekst i et dokument eller en kodebase. Dette kan være for å fikse skrivefeil, oppdatere gamle kodelinjer eller for å gjøre en rask endring i mange filer samtidig. I slike tilfeller kan det være nyttig å vite hvordan man enkelt kan søke og erstatte tekst ved hjelp av Kotlin.

## Slik gjør du det

For å søke og erstatte tekst i Kotlin, kan du bruke bruk av funksjonen `replace()` og `Regex`-klasse. Følgende kodeblokk viser et enkelt eksempel på hvordan dette kan gjøres:

```Kotlin
val originalString = "Hallo verden"
val newString = originalString.replace(Regex("verden"), "Universe")
println(newString) // Output: Hallo Universe
```

Her erstattes teksten "verden" med "Universe" i strengen "Hallo verden". Du kan også bruke variabler i søket og erstatningen, for eksempel:

```Kotlin
val firstName = "Sara"
val lastName = "Olsen"
val fullName = "$firstName $lastName" // Sara Olsen
val newFullName = fullName.replace(Regex(lastName), "Smith")
println(newFullName) // Output: Sara Smith 
```

Legg merke til hvordan man kan bruke variabler i Regex-uttrykket ved å bruke `$` foran variabelnavnet. Dette gjør det enkelt å oppdatere og endre søket hvis man trenger det.

## Dypdykk

Ved å bruke `Regex`-klassen, kan man også gjøre mer avanserte søk og erstatninger. For eksempel kan man søke etter uttrykk som starter på en bestemt bokstav:

```Kotlin
val originalString = "Banan, Bringebær, Blåbær, Jordbær"
val newString = originalString.replace(Regex("^B"), "F")
println(newString) // Output: Fanan, Fringebær, Flåbær, Fjordbær 
```

Søket vil her bare gjelde ord som starter på bokstaven "B", og disse vil bli erstattet med "F". Man kan også bruke `replaceFirst()`- og `replaceAll()`-funksjonene for å gjøre søket mer spesifikt, og man kan også bruke `Regex`-flagg for å gjøre søket ikke-eksakt eller mer komplekst.

## Se også

- [Kotlin regex tutorial](https://www.tutorialspoint.com/kotlin/kotlin_string_replace.htm)
- [Official Kotlin documentation](https://kotlinlang.org/docs/strings.html#string-regular-expressions)