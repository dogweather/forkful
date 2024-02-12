---
title:                "Refaktorering"
aliases:
- /no/javascript/refactoring.md
date:                  2024-01-26T01:41:39.392979-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Refaktorering er prosessen med å restrukturere eksisterende dataprogramkode uten å endre dens eksterne oppførsel. Programmerere gjør dette for å forbedre de ikke-funksjonelle attributtene til programvaren, noe som gjør koden renere og mer effektiv. Dette forenkler i sin tur vedlikehold og gjør fremtidige funksjonslegger enklere.

## Hvordan:

La oss se på et enkelt eksempel der refaktorering kan gjøre koden din mer konsis og lesbar. Her refaktorerer vi en funksjon som beregner summen av et tallarray.

Før:
```javascript
function calculateSum(arr) {
  let sum = 0;
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
  }
  return sum;
}

console.log(calculateSum([1, 2, 3, 4])); // Utdata: 10
```

Etter:
```javascript
function calculateSum(arr) {
  return arr.reduce((sum, num) => sum + num, 0);
}

console.log(calculateSum([1, 2, 3, 4])); // Utdata: 10
```

Ser du hvordan `reduce`-metoden reduserer størrelsen på funksjonen samtidig som funksjonaliteten beholdes? Det er refaktorering for deg.

## Dypdykk

Refaktorering dukket ikke opp som en formell praksis før publiseringen av Martin Fowlers bok "Refactoring: Improving the Design of Existing Code" i 1999. Denne boken, sammen med fremveksten av smidig programvareutvikling, hjalp med å skyve refaktorering inn i hovedstrømmen.

Å beskrive refaktorering som et aspekt ved programvareutvikling er som å forklare hvorfor du rydder opp i et verksted: du gjør det slik at neste gang du må fikse noe (i dette tilfellet, kode), vil du tilbringe mindre tid på å håndtere rotet og mer på det faktiske problemet.

Når vi snakker om alternativer til refaktorering, går vi inn i en bredere diskusjon om strategier for programvarevedlikehold. Man kunne for eksempel velge en fullstendig omskrivning, men det er ofte dyrere og risikofylt. Refaktorer inkrementelt, og du høster løpende fordeler uten å senke skipet fra en plutselig overhaling.

Refaktorering har blitt hjulpet av utviklingen av integrerte utviklingsmiljøer (IDEer) og verktøy som JSHint, ESLint og Prettier i JavaScript-økosystemet, som automatiserer kvalitetskontroller av kode og fremhever muligheter for refaktorering.

Det handler alt sammen om ren, uttrykksfull og vedlikeholdbar kode. Sofistikerte algoritmer, optimaliseringer av datastrukturer eller til og med arkitektoniske endringer som å bytte fra prosedyreorienterte til funksjonelle programmeringsstiler kan være en del av en refaktoreringsprosess.

Refaktorering må gjøres nøye; det er avgjørende å ha et robust sett med tester for å sikre at endringene dine ikke uventet har endret programmets oppførsel – enda en grunn til at Testdreven Utvikling (TDD) passer godt sammen med refaktorering, siden det gir det sikkerhetsnettet som standard.

## Se også

- Martin Fowlers Refaktoreringsbok: [Refaktorering - Forbedring av designet til eksisterende kode](https://martinfowler.com/books/refactoring.html)
- JavaScript Testrammeverk (for å sikre at refaktorering ikke bryter funksjonalitet):
  - Jest: [Jest - Deilig JavaScript-testing](https://jestjs.io/)
  - Mocha: [Mocha - det morsomme, enkle, fleksible JavaScript-testrammeverket](https://mochajs.org/)

- Verktøy for kodekvalitet og støtte til refaktorering:
  - ESLint: [ESLint - Pluggbar JavaScript-linter](https://eslint.org/)
  - Prettier: [Prettier - Prinsippfast kodeformaterer](https://prettier.io/)
