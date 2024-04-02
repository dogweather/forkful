---
date: 2024-01-20 17:46:13.912591-07:00
description: "Estrarre sottostringhe significa prendere parti specifiche di una stringa.\
  \ I programmatori lo fanno per analizzare, manipolare o trasformare i dati\u2026"
lastmod: '2024-03-13T22:44:43.800951-06:00'
model: gpt-4-1106-preview
summary: "Estrarre sottostringhe significa prendere parti specifiche di una stringa.\
  \ I programmatori lo fanno per analizzare, manipolare o trasformare i dati\u2026"
title: Estrazione di sottostringhe
weight: 6
---

## What & Why?
Estrarre sottostringhe significa prendere parti specifiche di una stringa. I programmatori lo fanno per analizzare, manipolare o trasformare i dati secondo le necessità.

## How to:
Ecco alcuni modi per estrarre sottostringhe in JavaScript:

```javascript
let stringa = "Ciao, mondo!";

// Metodo slice()
let sliceStr = stringa.slice(0, 5);
console.log(sliceStr); // Output: Ciao,

// Metodo substring()
let substrStr = stringa.substring(0, 5);
console.log(substrStr); // Output: Ciao,

// Metodo substr() (deprecato in ECMAScript 2020)
let substrDeprecatedStr = stringa.substr(0, 5);
console.log(substrDeprecatedStr); // Output: Ciao,
```

## Deep Dive
L'estrazione di sottostringhe in JavaScript si è evoluta. In passato, `substr()` era comunemente usato ma è stato deprecato a favore di `slice()` e `substring()`, entrambi con leggere differenze nella gestione degli indici negativi e di altri casi limite. Per esempio, `slice()` può accettare indici negativi, tornando elementi dalla fine della stringa, mentre `substring()` interpreta valori negativi come 0.

La scelta tra `slice()` e `substring()` dipenderà dal comportamento specifico che cerchi. Entrambi sono efficienti e facili da capire, ma `slice()` offre più flessibilità con gli indici negativi. A proposito di implementazione, queste funzioni lavorano con l'UTF-16 interno di JavaScript. Questo può generare problemi rari ma evidenti con caratteri fuori dal Basic Multilingual Plane (BMP).

## See Also
- MDN Web Docs su `slice()`: [String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- MDN Web Docs su `substring()`: [String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- Approfondimento sulla deprecazione di `substr()`: [String.prototype.substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
