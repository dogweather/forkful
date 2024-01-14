---
title:    "Javascript: Skriving av tester"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor
Å skrive tester i JavaScript kan virke tidkrevende og unødvendig, men det er faktisk en viktig del av å utvikle pålitelige og robuste programmer. Ved å skrive tester, kan du oppdage feil og bugs tidlig i utviklingsprosessen, noe som sparer deg for mye tid og frustrasjon i det lange løp.

## Slik gjør du det
For å skrive tester i JavaScript, kan du bruke et rammeverk som Mocha eller Jest. Disse rammeverkene gjør det enkelt å definere tester og kjøre dem automatisk.

La oss se på et eksempel med Jest:

```Javascript
// Funksjonen vår som skal testes:
function add(a, b) {
  return a + b;
}

// Testen:
test('add funksjonen skal legge til to tall', () => {
  expect(add(2, 2)).toBe(4);
});
```

Her bruker vi funksjonen `test` for å definere en ny test. Vi gir testen en beskrivelse og bruker `expect` for å sjekke om resultatet av `add`-funksjonen er 4 når vi gir den to tall som argumenter. Kjører vi denne testen vil den passere, men dersom vi endrer på funksjonen vår slik at den returnerer feil svar, vil testen feile og gi oss en indikasjon på at noe må fikses.

## Dypdykk
I tillegg til å teste funksjoner, kan du også skrive tester for brukergrensesnitt, API-er og mer komplekse deler av koden din. Ved å ha en god dekning av tester, kan du være trygg på at endringer og ny funksjonalitet du implementerer ikke ødelegger eksisterende funksjonalitet.

Det er også verdt å nevne at det å skrive tester også kan føre til en mer ryddig og strukturert kode. Når du må tenke på hva slags input funksjonene dine kan få og hva slags output de skal gi, kan det hjelpe deg med å skrive mer lesbar og oversiktlig kode.

## Se også
- [Mocha](https://mochajs.org/)
- [Jest](https://jestjs.io/)
- [https://www.testim.io/blog/testing-javascript/](https://www.testim.io/blog/testing-javascript/)
- [https://codeburst.io/learn-to-test-javascript-in-30-minutes-818a41aea05d](https://codeburst.io/learn-to-test-javascript-in-30-minutes-818a41aea05d)