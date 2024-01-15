---
title:                "Skriving av tester"
html_title:           "TypeScript: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av et effektivt utviklingsarbeid. Ved å skrive tester kan du sikre at koden din fungerer som den skal og oppdage eventuelle feil eller bugs tidlig, noe som sparer deg mye tid og frustrasjon i det lange løp.

Å skrive tester gir også bedre forståelse for koden din og hvordan ulike deler av den samhandler. Dette gjør det enklere å vedlikeholde og videreutvikle koden på sikt.

## Slik gjør du det

For å skrive tester i TypeScript, kan du bruke rammeverket Jest. Dette er et populært og enkelt å bruke verktøy for testing i JavaScript og TypeScript.

Først må du installere Jest ved å kjøre kommandoen `npm install jest --save-dev` i terminalen.

Deretter kan du begynne å skrive tester ved å lage en ny fil med navn `example.spec.ts` (`.spec` er vanligvis brukt for testfiler). I denne filen kan du skrive tester for forskjellige funksjoner eller komponenter i koden din.

For å definere en test, bruker du funksjonen `test` fra Jest. Inne i denne funksjonen kan du skrive selve testen ved hjelp av uttrykk som forventer et visst resultat fra koden din. Her er et eksempel på en enkel test:

```TypeScript
test('Should add two numbers together', () => {
  expect(add(2, 3)).toBe(5);
});
```

I dette eksempelet forventer vi at kallet `add(2, 3)` skal gi resultatet 5. Hvis det stemmer, vil testen passere. Hvis ikke, vil testen feile og vise hvilken verdi den faktisk fikk.

Du kan også skrive flere tester i samme fil, for forskjellige funksjoner eller situasjoner i koden din.

Etter å ha skrevet tester, kan du kjøre dem ved å kjøre kommandoen `npm test` i terminalen. Jest vil da kjøre alle tester i filen og gi deg en oversikt over hvilke som passerte og feilet.

## Dypdykk

I tillegg til å teste funksjoner og komponenter, kan du også skrive integrasjonstester for å sjekke at flere deler av koden din samhandler som de skal. Jest har også støtte for å mocke (simulere) forskjellige deler av koden din, for eksempel API-kall eller moduler.

Det finnes også andre rammeverk som kan brukes til testing i TypeScript, som for eksempel Mocha eller Jasmine. Det viktigste er å finne et verktøy som passer for deg og som du er komfortabel med å bruke.

## Se også

- [Jest dokumentasjon](https://jestjs.io/docs/en/getting-started)
- [Testing i TypeScript med Jest](https://dev.to/muhajir/testing-in-typescript-with-jest-2gln)
- [Introduksjon til testing med Jest i React-prosjekter](https://www.robinwieruch.de/react-testing-jest)