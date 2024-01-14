---
title:                "Javascript: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor skrive tester i Javascript

Mange av oss har vært der - vi skriver koden vår, tester den manuelt og alt ser ut til å fungere fint. Men så en dag, når en liten endring blir gjort, bryter hele koden sammen og vi angrer på at vi ikke skrev noen tester. Dette er hvorfor det er viktig å engasjere seg i å skrive tester i Javascript. Ved å skrive tester, sikrer vi at koden vår fungerer som den skal, og gir oss en følelse av trygghet ved å vite at endringer ikke vil forårsake uforutsette problemer.

## Hvordan skrive tester i Javascript

For å skrive tester i Javascript, kan vi bruke et rammeverk som Jest eller Mocha, som gir oss verktøy til å automatisere tester og generere rapporter. La oss ta en titt på et eksempel på hvordan vi kan skrive en enkel test ved hjelp av Jest:

```Javascript
// Unit test for addFunction
const addFunction = require('./myScript');
test('adds 2 + 2 to equal 4', () => {
  expect(addFunction(2, 2)).toBe(4);
});
```
Her oppretter vi en ny test som kjører funksjonen vår "addFunction" og forventer at resultatet skal være 4. Jest vil da sammenligne dette med det faktiske resultatet og gi oss en rapport om testen passerte eller mislyktes.

## Dypdykk i skriving av tester

Når vi skriver tester i Javascript, er det viktig å ha en god dekning av koden vår. Dette betyr at vi bør teste forskjellige deler av koden vår, inkludert kanttilfeller og feilhåndtering. Vi bør også prøve å skrive såkalte "isolerte tester", der hver test kun tester en liten del av koden vår for å sikre at det er lettere å finne og fikse eventuelle problemer.

En annen viktig ting å huske er å skrive lesbare tester. Dette vil ikke bare hjelpe oss med å forstå hva testene våre gjør, men også vil andre utviklere som jobber med koden vår forstå hva som blir testet og hvorfor.

# Se også

- [Jest dokumentasjon](https://jestjs.io/)
- [Mocha dokumentasjon](https://mochajs.org/)
- [Forskjellen mellom Jest og Mocha](https://www.testim.io/blog/jest-vs-mocha/)

Ved å følge disse tipsene og bruke verktøy som Jest eller Mocha, kan du enkelt integrere testing i din Javascript-kodebase og sikre at koden din er pålitelig og fungerer som den skal. Lykke til med å skrive tester i din Javascript-utvikling!