---
date: 2024-01-20 17:52:55.826173-07:00
description: "Hvordan: Bruk `console.log()` for \xE5 skrive ut verdier til konsollen.\
  \ Her er et eksempel."
lastmod: '2024-03-13T22:44:41.186093-06:00'
model: gpt-4-1106-preview
summary: "Bruk `console.log()` for \xE5 skrive ut verdier til konsollen."
title: "Skrive ut feils\xF8kingsdata"
weight: 33
---

## Hvordan:
Bruk `console.log()` for å skrive ut verdier til konsollen. Her er et eksempel:

```Javascript
let frukt = 'Eple';
console.log(frukt); // Skriver ut: Eple
```

Ønsker du mer komplisert datastruktur, bruk `JSON.stringify()`:

```Javascript
let bil = { merke: 'Tesla', modell: 'Model S' };
console.log(JSON.stringify(bil, null, 2));
// Output:
// {
//   "merke": "Tesla",
//   "modell": "Model S"
// }
```

For å fikse funksjoner, bruk `console.log()` inni funksjonen:

```Javascript
function add(a, b) {
  console.log(`Legger sammen: ${a} + ${b}`);
  return a + b;
}

let sum = add(5, 7);
// Konsoll: Legger sammen: 5 + 7
```

## Dykking:
Utskrift til konsollen for feilsøking har vært standard siden de gamle dager av programmering. Alternativer til `console.log()` inkluderer mer avanserte verktøy som debuggere, som lar deg pause kodekøringen og inspisere variabler. Implementasjonen av `console.log()` og lignende funksjoner kan variere mellom ulike JavaScript-motorer, som V8 (Chrome, Node.js) og SpiderMonkey (Firefox), men funksjonaliteten er stort sett lik.

For visning av tabellformat, bruk `console.table()`:

```Javascript
let folk = [{ navn: 'Erik', alder: 30 }, { navn: 'Anna', alder: 25 }];
console.table(folk);
```

For å filtrere loggnivåer etter viktighet, bruk funksjoner som `console.info()`, `console.warn()`, og `console.error()` for hhv. informativ, advarsel- og feilmeldingsutskrift.

## Se Også:
- MDN Web Docs - Console: https://developer.mozilla.org/en-US/docs/Web/API/console
- Node.js debugging guide: https://nodejs.org/en/docs/guides/debugging-getting-started/
- Chrome DevTools JavaScript debugger: https://developers.google.com/web/tools/chrome-devtools/javascript/
