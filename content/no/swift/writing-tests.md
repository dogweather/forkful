---
title:    "Swift: Å skrive tester"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av Swift-programmering. Det hjelper oss med å sikre at koden vår fungerer som den skal, og det kan også hjelpe oss med å identifisere og rette feil før de kommer til produksjon.

## Hvordan

Det er flere ulike måter å skrive tester på i Swift. En enkel måte er å bruke "Assert" metoden, som lar oss teste om et uttrykk er sant eller falskt.

```Swift
let x = 5
assert(x == 5, "x should equal 5")
```

Denne koden sjekker om verdien av "x" er lik 5, og returnerer en feilmelding hvis det ikke stemmer.

Vi kan også bruke "XCTest" rammeverket for å skrive mer omfattende tester. Her er et eksempel på en test som sjekker om en "Person" objekt har riktig navn og alder:

```Swift
func testPerson() {
    let person = Person(name: "Lars", age: 25)
    XCTAssertEqual(person.name, "Lars")
    XCTAssertEqual(person.age, 25)
}
```

Denne testen bruker "XCTAssertEqual" metoden for å sammenligne verdien av personens navn og alder med forventede verdier. Hvis testen feiler, vil den gi oss en feilmelding som hjelper oss med å identifisere årsaken til feilen.

## Dypdykk

I tillegg til å sjekke for riktige resultater, er det også viktig å skrive tester som dekker feil og unntakshåndtering. Dette kan gjøres ved å bruke "XCTAssertThrowsError" metoden, som lar oss teste om en bestemt blokk av kode kaster en feil.

Det er også viktig å skrive klare og godt strukturerte tester. Dette kan gjøres ved å følge prinsipper som "Arrange, Act, Assert", der vi først setter opp testmiljøet, deretter utfører en handling og til slutt sjekker resultatet.

## Se også

- [Swift Testing with XCTest](https://www.raywenderlich.com/960290-swift-testing-tutorial-getting-started)
- [Best Practices for Writing Swift Tests](https://medium.com/@gonzalezreal/best-practices-for-writing-swift-tests-9bc9b1a1de8c)
- [Unit Testing in Xcode](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/testing_with_xcode/chapters/01-introduction.html)