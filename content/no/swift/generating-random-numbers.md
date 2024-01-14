---
title:                "Swift: Generering av tilfeldige tall"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en vanlig oppgave i mange programmeringsspråk, og det kan være nyttig i en rekke situasjoner. Enten du trenger å simulere ulike scenarioer, lage tilfeldige passord eller til og med lage et enkelt spill, vil det å kunne generere tilfeldige tall være en nyttig ferdighet å ha i verktøykassen din.

## Hvordan du gjør det

Å generere tilfeldige tall i Swift er enkelt og kan gjøres på flere ulike måter, avhengig av dine behov. Det enkleste er å bruke standardbiblioteket i Swift sine innebygde funksjoner for å generere tilfeldige tall.

```Swift
// Genererer et tilfeldig tall mellom 0 og 10
let randomNumber = Int.random(in: 0...10)

// Genererer et tilfeldig flyttall mellom 0 og 100
let randomFloat = Float.random(in: 0...100)

// Genererer en tilfeldig boolsk verdi
let randomBool = Bool.random()
```

Du kan også spesifisere et område eller en liste av tall som du vil generere fra.

```Swift
// Genererer et tilfeldig tall mellom 50 og 100
let randomNumber = Int.random(in: 50...100)

// Genererer et tilfeldig tall fra en liste av tall
let numbers = [1, 3, 5, 7, 9]
let randomNumber = numbers.randomElement()
```

Du kan også bruke tilfeldig nummer generering til å velge en tilfeldig verdi fra en liste.

```Swift
let names = ["Marie", "Lars", "Ingrid", "Emil"]
let randomName = names.randomElement()
print("Den tilfeldige valgte navnet er \(randomName).")
// Output: Den tilfeldige valgte navnet er Marie.
```

## Dypdykk

Bak kulissene bruker Swifts standardbibliotek et tilfeldig tall generator algoritme kalt "mersenne twister" som sikrer at tallene som genereres er tilfeldige og ikke-prediktbare. Denne algoritmen krever en startverdi, kalt en seed, for å starte genereringen av tilfeldige tall. Hvis du ikke spesifiserer en seed, vil standardbiblioteket bruke datoen og klokkeslettet som seed.

Det er også mulig å lage en tilpasset generator ved å bruke Swifts RandomNumberGenerator protocol. Dette kan være nyttig hvis du trenger mer kontroll over hvilke tall som genereres eller ønsker å bruke en annen tilfeldig tall generator algoritme.

## Se også

For mer informasjon om tilfeldig tall generering i Swift, kan du sjekke ut følgende ressurser:

- [Swift's Standard Library Reference](https://developer.apple.com/documentation/swift/standard_library/random)
- [Hvordan generere tilfeldige tall i Swift](https://www.hackingwithswift.com/articles/38/how-to-generate-random-numbers-in-swift)
- [Implementasjon av tilfeldig nummer generering i Swift](https://medium.com/@darthpelo/implementing-a-random-number-generator-in-swift-part-1-a7381d3994df)