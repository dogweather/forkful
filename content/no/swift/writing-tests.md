---
title:                "Swift: Å skrive tester"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-tests.md"
---

{{< edit_this_page >}}

##Hvorfor

Det kan virke som en tidkrevende og unødvendig oppgave å skrive tester for koden din, men det kan være svært nyttig i det lange løp. Testene dine sørger for at koden din fungerer som den skal, og bidrar til å unngå feil og bugs i programmet. Det kan også gjøre det enklere for andre utviklere å forstå og videreutvikle koden din.

##Slik gjør du det

Å skrive tester i Swift er enkelt og kan gjøres ved å følge noen enkle trinn. Først må du legge til rammeverket XCTest i prosjektet ditt. Dette gir deg tilgang til Swift sitt eget testbibliotek.

Deretter kan du begynne å skrive tester ved hjelp av funksjonen "XCTAssert", som sjekker om en gitt betingelse er sann. Her er et eksempel på hvordan du kan teste om en funksjon returnerer riktig verdi:

```Swift
func addNumbers(_ num1: Int, _ num2: Int) -> Int {
   return num1 + num2
}

let result = addNumbers(5, 7)
XCTAssert(result == 12, "Resultatet skal være lik 12")
```

Når du kjører disse testene, vil du få en melding som sier at testen var vellykket eller feil, sammen med en beskrivelse av hva som gikk galt.

##Dykke dypere

Å skrive effektive og pålitelige tester handler om mye mer enn bare å bruke "XCTAssert". Du bør også vurdere ulike teststrategier, som f.eks. enhetstesting, integrasjonstesting og ytelsestesting. Å ha en god testdekning og å opprettholde testene dine regelmessig vil også være avgjørende for å sikre en stabil og pålitelig kodebase.

En annen viktig faktor å huske på når man skriver tester, er å holde koden din i små og uavhengige moduler. Dette gjør det enklere å teste hver del av koden separat, noe som gjør debugging og feilsøking enklere i fremtiden.

##Se også

- [XCTest dokumentasjon](https://developer.apple.com/documentation/xctest)
- [Code Coverage med XCTest](https://www.swiftbysundell.com/posts/code-coverage-with-swift-and-xctest)
- [Test Driven Development i Swift](https://medium.com/flawless-app-stories/test-driven-development-in-swift-for-absolute-beginners-3004cbe1bddb)