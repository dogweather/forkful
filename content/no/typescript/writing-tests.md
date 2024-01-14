---
title:    "TypeScript: Å skrive tester"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av enhver programmerers arbeidsflyt. Det hjelper til med å sikre at koden din fungerer som den skal, og identifiserer eventuelle potensielle feil før de når produksjonsfasen.

## Hvordan

For å skrive tester i TypeScript, følg disse enkle trinnene:

1. Installer et testbibliotek, for eksempel Jasmine eller Jest, ved hjelp av npm-kommandoen:
```TypeScript
npm install jasmine --save-dev
```
2. Lag en ny TypeScript-fil for testene dine, for eksempel "calculator.spec.ts".
3. Importer testbiblioteket i testfilen din:
```TypeScript
import jasmine from 'jasmine';
```
4. Lag en test-suit for å gruppere lignende tester sammen:
```TypeScript
describe('Kalkulator', () => {
    // Tester vil gå her
})
```
5. Opprett en test for hver funksjon du vil teste ved hjelp av "it" -funksjonen:
```TypeScript
it('Skal returnere riktig sum når to tall legges sammen', () => {
    // Lag en instans av kalkulatoren din og kall på funksjonen for å legge sammen to tall
    const kalkulator = new Kalkulator();
    const sum = kalkulator.leggTil(3, 5);
    // Sjekk at summen er riktig
    expect(sum).toBe(8);
})
```
6. Kjør testene ved å kjøre kommandoen:
```TypeScript
jasmine
```

Forventet utdata:
```bash
Kalkulator
    ✓ Skal returnere riktig sum når to tall legges sammen

1 spec, 0 failures 
```

## Dypdykk

Når du skriver tester, er det viktig å tenke på hvilke områder av koden din som er mest sannsynlig å feile, og å skrive tester for å dekke disse områdene. Det er også viktig å opprettholde og oppdatere testene dine etter hvert som koden din endres.

I tillegg kan det være nyttig å implementere testdrevet utvikling (TDD), der du først skriver tester og deretter koden som oppfyller disse testene. Dette kan bidra til å sikre at koden din er godt testet og fungerer som den skal.

## Se Også

- [Jasmine dokumentasjon](https://jasmine.github.io/)
- [Jest dokumentasjon](https://jestjs.io/)
- [TypeScript offisiell dokumentasjon](https://www.typescriptlang.org/docs/)