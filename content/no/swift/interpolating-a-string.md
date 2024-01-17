---
title:                "Interpolering av en streng"
html_title:           "Swift: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Hvis du er en programmerer, har du kanskje hørt begrepet "string-interpolering" før. Dette er en enkel måte å sette sammen en tekststreng ved å inkludere variabler, konstanter eller uttrykk i en tekst. Dette gjør det mulig å lage dynamiske tekststrenger som endrer seg basert på ulike inputs.

Interpolering av en string er nyttig fordi det gjør koden mer effektiv og lesbar. Istedenfor å måtte skrive ut alle delene av en tekststreng separat, kan du enkelt kombinere dem ved hjelp av interpolering. Dette sparer tid og reduserer risikoen for skrivefeil.

## Slik gjør du:
```Swift
let navn = "Maria"
let alder = 25
print("Hei, mitt navn er \(navn) og jeg er \(alder) år gammel.")
```
Output:
```
Hei, mitt navn er Maria og jeg er 25 år gammel.
```

Som du kan se i dette eksempelet, bruker vi bakoverstrek (\) for å inkludere variablene våre i teksten. Merk at det er viktig å bruke parentes rundt variablene for å få det til å fungere.

## Dykk dypere
Interpolering av strings har vært en del av Swift siden versjon 2, og har gjort det lettere for utviklere å produsere dynamiske tekststrenger. Det finnes også andre muligheter for å lage tekststrenger, som for eksempel ved hjelp av String-metoder eller formatere en streng ved hjelp av spesielle formateringskonstanter. Men interpolering anses som den mest effektive og letteste måten å oppnå dette på.

Når du jobber med interpolering, er det også nyttig å vite at du kan bruke escape-karakteret (\) for å forhindre at spesialtegn som anførselstegn eller line breaks ødelegger teksten din.

## Se også
Hvis du vil lære mer om string-interpolering og andre nyttige Swift-funksjoner, kan du sjekke ut disse kildene:

- [The Swift Programming Language (Swift i praksis)](https://docs.swift.org/swift-book/GuidedTour/GuidedTour.html)
- [Swift By Example - Strings (Swift ved å eksempel - Tekst)](https://www.hackingwithswift.com/example-code/strings/swift-by-example-string-interpolation)