---
date: 2024-01-26 01:10:39.136816-07:00
description: "Come fare: Storicamente, i linguaggi di programmazione imperativa come\
  \ le prime versioni di BASIC o Assembly mancavano dell'astrazione fornita dalle\u2026"
lastmod: '2024-04-05T21:53:44.566426-06:00'
model: gpt-4-1106-preview
summary: Storicamente, i linguaggi di programmazione imperativa come le prime versioni
  di BASIC o Assembly mancavano dell'astrazione fornita dalle funzioni.
title: Organizzazione del codice in funzioni
weight: 18
---

## Come fare:
```javascript
// Definire una funzione per calcolare l'area di un rettangolo
function calculateArea(larghezza, altezza) {
  return larghezza * altezza;
}

// Chiamare la funzione e stampare il risultato
let area = calculateArea(5, 3);
console.log(area); // Output: 15
```

```javascript
// Raggruppare funzionalità correlate utilizzando funzioni
function saluto(nome) {
  console.log(`Ciao, ${nome}!`);
}

function addio(nome) {
  console.log(`Addio, ${nome}!`);
}

saluto('Alice'); // Output: Ciao, Alice!
addio('Bob'); // Output: Addio, Bob!
```

## Approfondimento
Storicamente, i linguaggi di programmazione imperativa come le prime versioni di BASIC o Assembly mancavano dell'astrazione fornita dalle funzioni. Con il tempo, il concetto di codice modulare in linguaggi come C ha introdotto l'idea che suddividere il codice in unità (funzioni o procedure) porta a una migliore organizzazione e a una logica più chiara.

In JavaScript, oltre alle funzioni tradizionali, abbiamo le arrow function da ES6 (2015) che forniscono una sintassi più concisa e sono adatte per funzioni non-metodo.

Le alternative e i miglioramenti nell'organizzazione del codice in JavaScript includono approcci orientati agli oggetti utilizzando classi, o paradigmi di programmazione funzionale che trattano le funzioni come cittadini di prima classe.

Per quanto riguarda l'implementazione, le funzioni JavaScript supportano le chiusure (closures), fornendo un modo per mantenere l'accesso all'ambito di una funzione dopo l'esecuzione, il che è potente per l'incapsulamento e la creazione di funzioni factory, tra gli altri pattern.

## Vedi Anche
- MDN Web Docs sulle Funzioni: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions
- Design Pattern di JavaScript: https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- Codice Pulito JavaScript: https://github.com/ryanmcdermott/clean-code-javascript
