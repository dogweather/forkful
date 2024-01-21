---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:29.063933-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Generiamo numeri casuali per introdurre l'elemento dell'incertezza nei programmi. Questo è utile in giochi, simulazioni e tutto ciò che necessita una scelta imprevedibile.

## How to: (Come fare)
JavaScript offre un modo semplice per ottenere numeri casuali tramite il metodo `Math.random()`. Ecco come usarlo:

```javascript
// Genera un numero casuale tra 0 (incluso) e 1 (escluso)
let randomNumber = Math.random();
console.log(randomNumber); // esempio di output: 0.123456789

// Per un numero intero casuale tra 1 e 10
let randomInt = Math.floor(Math.random() * 10) + 1;
console.log(randomInt); // esempio di output: 5
```

## Deep Dive (Nel Profondo)
Prima di `Math.random()`, si doveva fare affidamento su algoritmi complessi o hardware specifico per i numeri casuali. In JavaScript, `Math.random()` non è adatto per la crittografia perché non è abbastanza casuale; in questo caso, considera `crypto.getRandomValues()`. Internamente, `Math.random()` utilizza un generatore di numeri pseudo-casuali (PRNG) per approssimare la casualità.

## See Also (Vedi Anche)
- MDN Web Docs su [`Math.random()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- Per usi crittografici, esplora [`crypto.getRandomValues()`](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
- Gli articoli sulla casualità e i PRNG, come ad esempio [Randomness in JavaScript](https://medium.com/@rossbulat/randomness-in-javascript-e564f7dede93)