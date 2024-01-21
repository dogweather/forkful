---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:53.196316-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Generering av tilfeldige tall brukes for å skape data som er uforutsigbar. Programmerere trenger dette for alt fra spillmekanikker til sikkerhetsfunksjoner.

## Hvordan:
Her er hvordan du genererer tilfeldige tall i Swift:

```Swift
// Genererer et tilfeldig tall fra 0 til 10
let randomInt = Int.random(in: 0...10)
print(randomInt)

// Genererer et tilfeldig tall med flyttall fra 0 til 1
let randomDouble = Double.random(in: 0..<1)
print(randomDouble)

// Genererer en tilfeldig Bool
let randomBool = Bool.random()
print(randomBool)
```
Eksempelutdata kan variere ettersom tallene er tilfeldige:
```
5
0.8654016930997391
true
```

## Dypdykk:
I tidlige dager brukte programmerere algoritmer som pseudo-tilfeldige nummergeneratorer (PRNG). Disse var ikke helt tilfeldige, men gode nok for mange brukstilfeller. Swift gjør det enklere med innebygde funksjoner, som `random(in:)`, som knytter seg til operativsystemets krypto-sterke tilfeldige nummergeneratorer.

Det finnes alternativer som `arc4random()` på eldre systemer, men Swifts `random` er foretrukket for sin enkelhet og sikkerhet. På detaljnivået brukes en form for entropi for å sikre at tallene er så uforutsigbare som mulig.

For spill og simuleringer hvor sekvensen av "tilfeldige" tall må reproduseres, kan en seedet generator være et godt valg.

## Se også:
- [Swift Documentation on Random Numbers](https://developer.apple.com/documentation/swift/randomnumbergenerator)
- [“Randomness” på Wikipedia for en grunnleggende forståelse](https://no.wikipedia.org/wiki/Tilfeldighet)
- [Apple's “GameplayKit” for mer avansert bruk av tilfeldighet i spill](https://developer.apple.com/documentation/gameplaykit)