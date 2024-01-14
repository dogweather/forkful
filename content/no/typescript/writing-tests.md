---
title:                "TypeScript: Skriver tester"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester i programmering er en viktig praksis for å sikre kvalitet og pålitelighet i koden din. Det kan hjelpe deg med å oppdage og fikse feil tidligere i utviklingsprosessen, og forhindre potensielle problemer i produksjon.

## Hvordan

Å skrive tester i TypeScript er enkelt og kan følge de samme grunnleggende retningslinjene som for andre programmeringsspråk. Først må du installere et testrammeverk som Jest, Mocha eller Karma. Deretter kan du skrive tester ved hjelp av "describe" og "it" funksjonene, som lar deg organisere testene dine i grupper og gi beskrivelser for hver test. Du kan også bruke "expect" funksjonen for å teste om en bestemt verdi er lik forventet verdi. Se eksempelet nedenfor:

```TypeScript
describe('Kalkulator funksjoner', () => {
  it('Skal returnere riktig sum', () => {
    const a = 5;
    const b = 10;
    expect(a + b).toBe(15);
  });
});
```

Dette er et veldig enkelt eksempel, men det viser hvordan du kan strukturere og skrive en test i TypeScript. Du kan også bruke flere avancertere funksjoner og metoder som "beforeEach" for å håndtere initialisering av variabler eller "mock" for å simulere eksterne ressurser.

## Deep Dive

For å skrive effektive tester i TypeScript, er det viktig å forstå begrepet "assertions". Dette er en metode for å teste om en verdi er sann eller ikke. TypeScript støtter flere forskjellige assertions som "toBe" som vi brukte i eksempelet ovenfor, "toEqual" for å sammenligne objekter og "toBeTruthy" for å teste om en verdi er sann. Du kan også bruke "expect" med "toThrow" for å teste om en funksjon kaster en forventet feil. Det er viktig å velge riktig assertions for det du tester for å sikre nøyaktige resultater.

I tillegg er det viktig å følge god praksis når det gjelder å skrive tester. Du bør alltid skrive tester som er uavhengige av hverandre, slik at en feil i en test ikke påvirker resultatet av andre tester. Det er også viktig å holde testene dine enkle og leselige, slik at du enkelt kan forstå hva som blir testet og finne ut av eventuelle problemer.

## Se også

- [Jest](https://jestjs.io/)
- [Mocha](https://mochajs.org/)
- [Karma](https://karma-runner.github.io/latest/index.html)