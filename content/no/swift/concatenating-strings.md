---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Banalt, men viktig: konkatenering av strenger er prosessen med å sette to eller mer strenger sammen for å lage en enkelt. Vi gjør det for å bygge eller manipulere setninger effektivt i programmering.

## Hvordan Gjøre:
I Swift, vi kan slå sammen strenger på flere måter. Enkel og intuitiv metode er ved å bruke "+" operator. Her er et eksempel:

```swift
let fornavn = "Ola"
let etternavn = "Nordmann"
let fullNavn = fornavn + " " + etternavn
print(fullNavn)  //Outputs "Ola Nordmann"
```

Vi kan også bruke pump-operatoren "+=" for å legge til en streng til en eksisterende.

```swift
var godMorgen = "God morgen, "
godMorgen += "verden"
print(godMorgen)  // Outputs "God morgen, verden"
```

## Dyp Dykk:
Historisk kontekst: Strengkonkatenering har vært en standardfunksjon i de fleste programmeringsspråk siden de tidligste dagene av programmering.

Alternativer: Andre enn "+", kan vi bruke String Interpolation, spesielt når du kombinerer strenger med andre datatyper.

```swift
let time = 20
let greeting = "Det er nå klokka \(time)"
print(greeting)  //Outputs "Det er nå klokka 20"
```

Implementeringsdetaljer: Swift optimaliserer strengkonkatenering i bakgrunnen. I stedet for å lage en ny streng hver gang du legger til en underliggende streng, refererer Swift bare til originalene. Dette gjør prosessen rask og effektiv.

## Se Også:
1. Swift dok: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)