---
title:                "TypeScript: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig og standard praksis i programméring for å sikre kvalitet og pålitelighet i koden din. Ved å skrive tester, kan du identifisere og fikse feil før koden din blir utgitt, noe som gir en bedre brukeropplevelse og sparer tid og ressurser på lang sikt.

## Hvordan

For å skrive tester i TypeScript, må du først opprette en testfil. Denne filen bør ha samme navn som filen du skal teste, men med ".spec" lagt til. For eksempel, hvis filen din heter "calculator.ts", burde testfilen være "calculator.spec.ts".

```TypeScript
// calculator.spec.ts

import { add } from './calculator';

describe('Testing Calculator', () => {
  it('should add two numbers correctly', () => {
    const result = add(2, 4);
    
    expect(result).toBe(6);
  })
})
```

I denne koden bruker vi "describe" og "it" fra Jasmine testing biblioteket for å opprette en test suite og en enkelt test. Vi importerer også funksjonen vi vil teste fra den opprinnelige filen og bruker "expect" og "toBe" for å sjekke om resultatet er som forventet.

## Dypdykk

Å skrive effektive tester innebærer å teste alle mulige scenarioer, inkludert kanttilfeller og feil. Dette sikrer at koden din er robust og håndterer alle situasjoner som kan oppstå. Det er også viktig å sørge for at testene dine er uavhengige av hverandre, slik at en feil i en test ikke påvirker resultatet av en annen test.

Det er også viktig å implementere en kontinuerlig integreringsprosess for å sikre at testene dine kjører automatisk hver gang du gjør en endring i koden din. Dette vil hjelpe deg med å oppdage feil raskere og gjøre det lettere å rette dem.

## Se også

- [Jasmine dokumentasjon](https://jasmine.github.io/)
- [En introduksjon til testing i TypeScript](https://mariusschulz.com/articles/a-gentle-introduction-to-testing-in-typescript)
- [En guide til effektiv testing i JavaScript](https://blog.logrocket.com/a-quick-and-complete-guide-to-mocha-testing-d0e0ea09f09d/)