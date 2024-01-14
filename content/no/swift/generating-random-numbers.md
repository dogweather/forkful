---
title:    "Swift: Generere tilfeldige tall"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Generering av tilfeldige tall er en viktig del av mange programmeringsoppgaver. Enten det er for å lage spill, tilfeldige passord eller tester, er det å kunne generere tilfeldige tall et nyttig verktøy i en utviklers verktøykasse. I denne bloggposten vil vi gå gjennom hvordan man kan generere tilfeldige tall i Swift.

## Hvordan
For å generere tilfeldige tall i Swift, kan vi bruke funksjonen `random(in:)` fra Swifts `Random` bibliotek. Denne funksjonen tar inn et område av tall som parameter, og vil returnere et tilfeldig tall innenfor dette området.

Her er et eksempel på hvordan vi kan bruke `random(in:)` for å generere et tilfeldig tall mellom 1 og 10:

```Swift
let randomIndex = Int.random(in: 1...10) 
print(randomIndex) // Output: 7
```

Vi kan også bruke denne funksjonen til å generere tilfeldige desimaltall. For å generere et tilfeldig desimaltall mellom 0 og 1, kan vi bruke følgende kode:

```Swift
let randomDecimal = Double.random(in: 0..<1)
print(randomDecimal) // Output: 0.345
```

Vi kan også generere tilfeldige elementer fra en liste ved hjelp av `randomElement()` funksjonen. Denne funksjonen tar inn en liste og returnerer et tilfeldig element fra listen.

```Swift
let fruits = ["apple", "orange", "banana", "strawberry"]
let randomFruit = fruits.randomElement()
print(randomFruit) // Output: apple (or any other fruit in the list)
```

Det finnes også andre metoder for å generere tilfeldige tall i Swift, som for eksempel `arc4random_uniform()`, men disse er ikke like sikre og anbefales derfor ikke.

## Deep Dive
Når vi genererer tilfeldige tall i Swift, bruker vi egentlig en pseudorandom nummergenerator. Dette betyr at tallene som genereres ikke er helt tilfeldige, men følger et bestemt mønster. Grunnen til dette er at datamaskiner ikke kan produsere helt tilfeldige tall, men de kan simulere det gjennom komplekse matematiske formler og algoritmer.

For å sikre at tallene som genereres er så tilfeldige som mulig, kan vi bruke en såkalt "seed" som en parameter i funksjonene `random(in:)` og `randomElement()`. Seeden er et tall som brukes for å initialisere nummergeneratoren og derfor påvirker resultatet av de tilfeldige tallene. Ved å endre seed tallet, vil vi få en annen sekvens av tilfeldige tall. Dette kan være nyttig i enkelte programmeringsscenarier.

## Se også
- [Swifts offisielle dokumentasjon om tilfeldige tall](https://developer.apple.com/documentation/swift/random)
- [En guide til tilfeldige tall i Python](https://realpython.com/python-random/)
- [En forklaring på pseudorandom nummergeneratorer](https://www.khanacademy.org/computing/computer-science/cryptography/crypt/v/random-vs-pseudorandom-number-generators)