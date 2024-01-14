---
title:                "Swift: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor bry seg med å generere tilfeldige tall? Vel, tilfeldige tall kan være nyttige i mange ulike situasjoner innen programmering. De kan for eksempel brukes til å generere unike ID-numre, til å velge tilfeldige elementer fra en liste, eller til å lage realistiske simuleringer og spill.

## Slik gjør du det
For å generere et tilfeldig tall i Swift, kan du bruke funksjonen `arc4random()`, som vil returnere et tilfeldig tall av typen `UInt32`. Dette er et stort heltall, så det kan være lurt å konvertere det til et mindre tall ved å bruke modulus-operatøren (`%`).

```
let randomNumber = arc4random() % 100 // Dette vil generere et tall mellom 0 og 99
```

Du kan også bruke `arc4random_uniform()`-funksjonen hvis du ønsker å generere tilfeldige tall innenfor et spesifikt område uten å måtte bruke modulus-operatøren.

```
let randomNumber = arc4random_uniform(10) // Dette vil generere et tall mellom 0 og 9
```

For å generere tilfeldige elementer fra en liste, kan du bruke `.randomElement()`-metoden som er tilgjengelig på arrayer og set-typer.

```
let fruits = ["eple", "banan", "appelsin", "melon", "jordbær"]
let randomFruit = fruits.randomElement() // Dette vil velge et tilfeldig element fra listen
```

## Dykk dypere
`arc4random()`-funksjonen bruker en algoritme for å generere tall basert på systemtiden. Dette betyr at tallene som genereres ikke er helt tilfeldige, men kan være forutsigbare hvis du vet når funksjonen ble kalt. Hvis du har behov for sikrere tilfeldige tall, kan du bruke `arc4random_uniform()` eller andre tilfeldighetsgenererende algoritmer som er tilgjengelige i Swift.

## Se også
- [Offisiell dokumentasjon for tilfeldige tall i Swift](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID331)
- [Stack Overflow-innlegg om tilfeldige tall i Swift](https://stackoverflow.com/questions/24007129/how-does-arc4random-work-can-it-be-made-more-random)
- [Generere tilfeldige tall i andre programmeringsspråk](https://blog.hubspot.com/website/random-number-generator)