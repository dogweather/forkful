---
title:                "Skriver tester"
html_title:           "TypeScript: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrivetesting er en viktig del av utviklingsprosessen for en programmerer. Det er en praksis som innebærer å skrive kode som tester ulike funksjoner og aspekter av et program for å sikre at alt fungerer som det skal. Dette er viktig for å oppdage eventuelle feil og bugs før programmet blir utgitt.

## Slik gjør du det:
```TypeScript
describe('Enkel kalkulator funksjon', () => {
  it('Legger sammen to tall', () => {
    const sum = add(2, 3);
    expect(sum).toBe(5);
  })
})

function add(a, b) {
  return a + b;
}
```
I dette eksempelet tester vi en enkel kalkulator funksjon ved å sjekke om den returnerer riktig verdi når vi legger sammen to tall. Ved å bruke testrammeverket Jest og den innebygde funksjonen `expect`, kan vi enkelt sjekke om funksjonen virker som den skal.

## Dypdykk:
Det å teste kode er en viktig del av utviklingsprosessen fordi det sikrer kvaliteten på programmet og gjør det mindre sårbart for feil. Alternativet til å skrive tester er å manuelt teste programmet hver gang det endres, noe som kan være tidskrevende og lede til oversette feil. Det finnes forskjellige testrammeverk og biblioteker for TypeScript, men Jest er et populært valg på grunn av sin enkelhet og støtte for flere funksjoner som asynkrone tester.

## Se også:
- [Jest dokumentasjon](https://jestjs.io/docs/getting-started)
- [Why Writing Tests Is Important?](https://www.business.com/articles/why-writing-tests-is-important/)
- [Alternatives to Jest for TypeScript testing](https://medium.com/agoldis-one/alternatives-to-jest-for-typescript-testing-d2f6d419ce3d)