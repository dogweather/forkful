---
title:    "Swift: Skriver til standardfeil"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error kan være nyttig for å feilsøke og forbedre programmet ditt. I stedet for å bare skrive ut feilmeldinger til vanlig utgang, kan du skrive dem til standard error for å få mer detaljert og tydelig informasjon om hva som er galt. Dette kan hjelpe deg med å finne og rette feil raskere.

## Hvordan du gjør det

Du kan enkelt skrive til standard error i Swift ved å bruke `print()` funksjonen og spesifisere `to: .standardError` som parameter. Her er et eksempel på hvordan du kan gjøre dette i et program som regner ut gjennomsnittet av to tall:

```Swift
let tall1 = 10
let tall2 = 5

let gjennomsnitt = (tall1 + tall2) / 2

if gjennomsnitt < 4 {
  print("Gjennomsnittet er for lavt", to: .standardError)
} else {
  print("Gjennomsnittet er \(gjennomsnitt)")
}
```

Denne koden vil skrive ut feilmeldingen "Gjennomsnittet er for lavt" til standard error dersom gjennomsnittet er mindre enn 4. Ellers vil den skrive ut gjennomsnittet som en vanlig utgang.

## Dypdykk

I tillegg til å gi mer detaljert informasjon, kan skriving til standard error også være nyttig for å skille mellom forskjellige typer meldinger som skrives ut. Dette kan gjøres ved å bruke en egen `print()` funksjon som spesifiserer `.standardError` som parameter. Dette gjør det mulig å filtrere ut eller håndtere standard error-data separat fra annen utgang.

Et annet bruksområde for skriving til standard error er å fange feilmeldinger og håndtere dem på en bestemt måte. Dette kan være spesielt nyttig når du arbeider med nettverk, filer eller andre eksterne ressurser.

## Se også

- [Error handling in Swift](https://blog.nordiskconetent.com/error-handling-in-swift)
- [Printing to standard error in Swift](https://www.swiftbysundell.com/articles/printing-to-standard-error-in-swift)
- [Standard streams](https://en.wikipedia.org/wiki/Standard_streams)