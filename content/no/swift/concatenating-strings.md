---
title:    "Swift: Sammenslåing av strenger"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slå sammen strenger er en viktig del av mange programmeringsoppgaver, spesielt når det kommer til å håndtere tekstdata. Dette kan være nyttig når du ønsker å kombinere forskjellige tekststrukturer eller legge til dynamisk generert tekst i et program. Ved å lære å slå sammen strenger i Swift, vil du kunne utføre komplekse tekstmanipulasjoner og bygge mer robuste programmer.

## Hvordan

For å slå sammen strenger i Swift, kan du bruke operatøren "+" for å kombinere to eksisterende strenger til en ny. For eksempel, hvis du ønsker å slå sammen en fornavn og etternavn, kan du skrive følgende kode:

```Swift
let fornavn = "Marie"
let etternavn = "Johansen"
let fulltNavn = fornavn + etternavn
```

Her vil verdiene av variablene "fornavn" og "etternavn" kombineres og lagres i "fulltNavn" variabelen. Du kan også slå sammen flere strenger samtidig ved å bruke operatøren flere ganger. For eksempel, hvis du ønsker å legge til et mellomrom og en ekstra streng, kan du gjøre følgende:

```Swift
let fornavn = "Marie"
let mellomrom = " "
let etternavn = "Johansen"
let fulltNavn = fornavn + mellomrom + etternavn
```

Resultatet vil bli "Marie Johansen". Du kan også bruke operatøren på en kombinasjon av variabler og konstante verdier.

## Dypdykk

Når du slår sammen strenger, er det viktig å være klar over datatyper. Hvis du for eksempel prøver å slå sammen en streng og et tall, vil dette ikke fungere, da datatypene er forskjellige. Du kan imidlertid konvertere tall til strenger ved å bruke Swifts "String" -type, for eksempel:

```Swift
let alder = 27
let alderTekst = String(alder) // alderTekst vil nå være en streng som sier "27"
```

Du kan også formatere strenger ved hjelp av "String(format:" %d ",)" funksjonen for å lage ønsket utgang. Det finnes også en rekke andre funksjoner og metoder for å arbeide med og manipulere strenger, som du kan utforske nærmere i Swifts offisielle dokumentasjon.

## Se Også

- Swift dokumentasjon: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Video tutorial om å slå sammen strenger: https://youtu.be/llZEtOQODlI
- I dybden artikkel om strenger i Swift: https://www.hackingwithswift.com/articles/140/what-is-a-string-in-swift