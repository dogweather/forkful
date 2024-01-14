---
title:    "TypeScript: Å skrive tester"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

Hvorfor teste kode i TypeScript?

Å skrive tester er en viktig del av programmeringsprosessen og kan hjelpe til med å sikre at koden vår fungerer som den skal. Selv om det kan føles tidkrevende å skrive tester, kan det spare mye tid og frustrasjon i det lange løp.

## Hvordan teste kode i TypeScript

For å skrive tester i TypeScript, kan vi bruke et testrammeverk som Jest. La oss ta en nærmere titt på et eksempel:

```TypeScript
//importerer funksjonen vi skal teste
import {addNumbers} from './utils';

//tester funksjonen addNumbers()
test('should add two numbers', () => {
  //forventet resultat
  const expected = 6;
  //faktiske resultater
  const result = addNumbers(2, 4);
  //sjekker om resultatet er det samme som forventet
  expect(result).toEqual(expected);
});
```

I dette eksemplet har vi en funksjon som legger sammen to tall, og vi tester om den gir riktig resultat. Vi importerer funksjonen vi vil teste og bruker testrammeverket Jest for å kjøre testen. Ved å kjøre testen får vi et resultat som indikerer om testen passerte eller feilet.

## Dykk dypere inn i testing

Det er viktig å forstå konseptet bak testing, slik at vi kan skrive effektive og pålitelige tester. Noen viktige begreper å være kjent med er enhetstesting, integrasjonstesting og end-to-end-testing. Enhetsstesting fokuserer på å teste en liten del av koden isolert, mens integrasjonstesting tester hvordan forskjellige deler av koden fungerer sammen. End-to-end-testing tester hele applikasjonen, inkludert grensesnittet.

En annen viktig del av testing er å dekke så mye kode som mulig. Dette kan gjøres ved å bruke kodeanalyseverktøy som Istanbul for å se hvilke deler av koden som ikke er testet, og deretter skrive flere tester for å dekke disse områdene.

## Se også

- [Jest - Documentation](https://jestjs.io/docs/getting-started)
- [Istanbul - Code Coverage Tool](https://istanbul.js.org/)
- [TypeScript - Unit Testing Basics with Jest](https://www.hackdoor.io/articles/e86DaVAA/unit-testing-basics-with-jest)