---
title:                "Generering av tilfeldige tall"
html_title:           "Swift: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Å generere tilfeldige tall er en vanlig oppgave i programmering, og det kan brukes til ulike formål som å lage spill, testing eller tilfeldige valg. 

## Slik gjør du det
Det er enkelt å generere tilfeldige tall i Swift ved å bruke rand() eller arc4random() funksjonene. Her er et eksempel på hvordan du kan generere et tilfeldig tall mellom 1 og 10 og skrive det ut i konsollen:

```Swift
let randomNumber = Int.random(in: 1...10)
print(randomNumber)
```

Dette vil gi forskjellige tall hver gang koden kjøres. Du kan også begrense tallene til et bestemt intervall ved å endre området i Int.random() funksjonen.

## Dypdykk
I Swift, er funksjonen rand() en del av C standard library, mens arc4random() er en nyere og sikrere versjon som bruker en kryptografisk tilfeldig tallgenerator. Det er også mulig å generere tilfeldige flyttall ved å bruke random() funksjonen. 

En annen nyttig metode er å bruke shuffle() for å blande en liste med tall i tilfeldig rekkefølge.

For å gjøre genereringen av tall enda mer tilfeldig, kan du også bruke seed() funksjonen for å endre startpunktet for de tilfeldige tallene.

## Se også
- [Swift Programmeringsspråk](https://developer.apple.com/swift/)
- [Offisiell Swift Dokumentasjon](https://docs.swift.org/swift-book/)
- [Tilfeldige tall i Swift dokumentasjonen](https://developer.apple.com/documentation/swift/numeric/2896015-random)