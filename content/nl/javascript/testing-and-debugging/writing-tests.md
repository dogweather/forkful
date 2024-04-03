---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:58.054077-07:00
description: 'Hoe: Stel je een eenvoudige functie voor om twee getallen op te tellen
  in JavaScript.'
lastmod: '2024-03-13T22:44:51.205360-06:00'
model: gpt-4-0125-preview
summary: Stel je een eenvoudige functie voor om twee getallen op te tellen in JavaScript.
title: Tests Schrijven
weight: 36
---

## Hoe:
Stel je een eenvoudige functie voor om twee getallen op te tellen in JavaScript:

```javascript
function add(a, b) {
  return a + b;
}
```

Om dit te testen, zou je een testframework zoals Jest kunnen gebruiken. Hier is hoe je een basis test zou schrijven:

```javascript
const add = require('./add'); // uitgaande dat de add functie in 'add.js' staat

test('voegt 1 + 2 samen om 3 te worden', () => {
  expect(add(1, 2)).toBe(3);
});
```

Voer de tests uit, en Jest zal je vertellen of de `add` functie geslaagd is:

```plaintext
PASS  ./add.test.js
✓ voegt 1 + 2 samen om 3 te worden (5ms)
```

## Diepere Duik
Historisch gezien was testen manueel, vervelend en foutgevoelig. De opkomst van geautomatiseerd testen in de late 20e eeuw verbeterde dit, met TDD (Testgedreven Ontwikkeling) als een sleutelmethodologie. Alternatieven voor Jest omvatten Mocha, Jasmine en QUnit, onder anderen. Het voornaamste uitvoeringsdetail bij het schrijven van tests is de bewering: een uitspraak die controleert of iets waar is. Als beweringen slagen, is je test geslaagd.

## Zie Ook
- Jest: https://jestjs.io/
- Testgedreven Ontwikkeling: https://nl.wikipedia.org/wiki/Testgedreven_ontwikkeling
- Mocha: https://mochajs.org/
- Jasmine: https://jasmine.github.io/
- QUnit: https://qunitjs.com/
