---
title:    "Javascript: Skriving av tester"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester som en del av utviklingsprosessen er en viktig praksis for å sikre kvalitet og pålitelighet i koden din. Tester hjelper deg med å identifisere og fikse feil i koden tidlig, redusere risikoen for at uønskede endringer blir gjort, og gjør det enklere for deg å vedlikeholde og videreutvikle koden din.

## Hvordan

Her er et enkelt eksempel på hvordan du kan skrive en enkel test i Javascript ved hjelp av Jest-testrammeverket:

```Javascript
// Kodestump: Funksjonen som skal testes
function addNumbers(a, b) {
  return a + b;
}

// Tester: Vi forventer at funksjonen returnerer riktig sum
test('Skal returnere riktig sum', () => {
  expect(addNumbers(2, 3)).toBe(5);
});
```

I dette eksempelet bruker vi testfunksjonen `expect` fra Jest for å sjekke om resultatet av funksjonen `addNumbers` er lik verdien 5 når vi gir inn argumentene 2 og 3. Dette er en enkel måte å teste en funksjon på og gir oss en rask tilbakemelding på om koden fungerer som forventet.

I tillegg til å bruke testrammeverk som Jest, kan du også skrive egne tester ved å sjekke forventede resultater mot faktiske resultater, og ved å sjekke om funksjoner utfører ønsket oppførsel.

## Dypdykk

Å skrive tester kan virke tidkrevende og unødvendig i begynnelsen, men det kan faktisk spare deg for mye tid og arbeid på lang sikt. Gjennom å skrive tester, lærer du deg å tenke mer gjennom grundig på koden din og forbedre kvaliteten på den. Det kan også hjelpe deg med å identifisere dårlige designvalg og gjøre koden din mer lesbar og vedlikeholdbar.

Det er også viktig å huske på at tester ikke garanterer feilfri kode, men de kan hjelpe deg med å avdekke potensielle feil og gi deg en større følelse av trygghet når du gjør endringer i koden din.

## Se Også

- [Jest](https://jestjs.io/) - Testrammeverk for Javascript
- [Testing JS](https://www.testingjavascript.com/) - Ressurser og kurs for å lære mer om testing i Javascript
- [10 Mest Vanlige Feil i Testkode og Hvordan Unngå Dem](https://codeutopia.net/blog/2017/11/10-common-unit-testing-mistakes/) - Tips for å skrive bedre tester.