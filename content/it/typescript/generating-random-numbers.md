---
title:                "TypeScript: Generazione di numeri casuali"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'abilità utile per molti programmatori. Può essere utilizzato in una varietà di progetti, come giochi, simulazioni, o anche solo per scopi di test.

## Come

Per generare numeri casuali in TypeScript, possiamo utilizzare la funzione `Math.random()` che restituisce un numero casuale compreso tra 0 (incluso) e 1 (escluso). Possiamo moltiplicare questo numero per il nostro intervallo di interesse, ad esempio 10 per ottenere un numero compreso tra 0 e 10. Per ottenere un numero intero invece, possiamo utilizzare la funzione `Math.floor()` per arrotondare il numero verso il basso.

```TypeScript
// Generare un numero casuale tra 0 e 10
let randomNumber = Math.random() * 10;
console.log(randomNumber);

// Generare un numero intero tra 1 e 100
let randomInteger = Math.floor(Math.random() * 100) + 1;
console.log(randomInteger);
```

L'output di questo codice potrebbe essere, ad esempio, `7.23545432` come numero casuale e `45` come numero intero.

## Approfondimento

Mentre `Math.random()` è un modo semplice per generare numeri casuali in TypeScript, ci sono anche altre opzioni disponibili per controllare meglio la distribuzione dei numeri generati. Per esempio, la libreria `random-js` offre una maggiore varietà di funzioni e opzioni per la generazione di numeri casuali, come ad esempio la possibilità di specificare una distribuzione gaussiana o una distribuzione uniforme.

Per una maggiore sicurezza e imprevedibilità dei numeri casuali, possiamo utilizzare anche il modulo `crypto` di Node.js che offre una funzione `randomBytes()` che restituisce una sequenza di byte casuale che può essere convertita in numeri.

## Vedi anche

- [Documentazione di Math.random()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Libreria random-js](https://www.npmjs.com/package/random-js)
- [Documentazione di Node.js - Modulo crypto](https://nodejs.org/api/crypto.html)