---
title:    "Swift: Generering av tilfeldige tall"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor vi trenger å generere tilfeldige tall

Generering av tilfeldige tall er en viktig del av programmering, spesielt når man utvikler spill eller applikasjoner som krever en element av tilfeldighet. Det kan også være nyttig i testing og simuleringer. I denne bloggposten vil vi utforske hvordan man kan generere tilfeldige tall i Swift.

## Slik genererer du tilfeldige tall i Swift

For å generere tilfeldige tall i Swift, kan vi bruke funksjonen `arc4random()`. Denne funksjonen returnerer et tilfeldig tall av typen `UInt32` som kan konverteres til andre typer tall. For eksempel, hvis vi ønsker et tilfeldig tall mellom 1 og 10, kan vi bruke følgende kode:

```Swift
let randomNum = arc4random_uniform(10) + 1
print(randomNum) // output: 5
```

Vi bruker `arc4random_uniform()` for å begrense det genererte tallet til å være mellom 1 og 10. Vi legger også til 1 for å unngå at tallet blir 0.

## Dypdykk i generering av tilfeldige tall

Det finnes flere måter å generere tilfeldige tall på i Swift. I tillegg til `arc4random()`, kan man også bruke `arc4random_uniform()` med større tall for å få et bredere utvalg av tilfeldige tall. Det finnes også andre funksjoner som `random()` og `random(in:)` som kan brukes til å generere tilfeldige tall innenfor et spesifikt område.

Det er også viktig å huske at tilfeldige tall faktisk ikke er helt tilfeldige, men genereres ved hjelp av en algoritme. Derfor er det viktig å bruke en god kilde til "entropi" (tilfeldighet) for å få så tilfeldige tall som mulig.

## Se også

- [Random Numbers in Swift](https://www.swiftbysundell.com/posts/random-numbers-in-swift) by Swift by Sundell
- [Swift - How to generate random number within a given range](https://stackoverflow.com/questions/24007129/swift-random-number-generator-between-two-numbers) on Stack Overflow
- [arc4random() documentation](https://developer.apple.com/documentation/swift/1574064-arc4random) on Apple Developer website