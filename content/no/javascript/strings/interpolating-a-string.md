---
date: 2024-01-20 17:50:59.943159-07:00
description: "Hvordan gj\xF8re det: I gamle JavaScript brukte vi '+' for \xE5 sette\
  \ sammen strenger og variabler. Etter ES6 (ECMAScript 2015) innf\xF8rte vi template\
  \ literals,\u2026"
lastmod: '2024-04-05T22:50:55.175992-06:00'
model: gpt-4-1106-preview
summary: "I gamle JavaScript brukte vi '+' for \xE5 sette sammen strenger og variabler."
title: Interpolering av en streng
weight: 8
---

## Hvordan gjøre det:
```javascript
// Eksempel med Template Literals (ES6+)
const navn = 'Ola';
const hilsen = `Hei, ${navn}! Hvordan har du det?`;
console.log(hilsen);  // Output: Hei, Ola! Hvordan har du det?

// Gammel metode med konkatenasjon
const gammelHilsen = 'Hei, ' + navn + '! Hvordan har du det?';
console.log(gammelHilsen);  // Output: Hei, Ola! Hvordan har du det?
```

## Dypdykk
I gamle JavaScript brukte vi '+' for å sette sammen strenger og variabler. Etter ES6 (ECMAScript 2015) innførte vi template literals, som gjør det raskere og ryddigere. I stedet for å bruke plusstegn, omslutter du strengen med backticks (\`) og plasserer variabler eller uttrykk inni `${}`. Dette er mer fleksibelt siden du også kan sette inn uttrykk, kalle funksjoner og til og med ha flerlinjetekst uten ekstra arbeid. Det finnes alternativer som `sprintf` i andre språk, men i JavaScript holder template literals stort sett mål.

## Se også
- MDN Web Docs om template literals: [MDN Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- Du kan også utforske biblioteker som `lodash` for ekstra string-manipuleringsfunksjoner: [Lodash](https://lodash.com/docs/#template)
