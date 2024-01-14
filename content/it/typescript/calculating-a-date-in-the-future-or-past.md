---
title:    "TypeScript: Calcolare una data nel futuro o nel passato"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel passato o nel futuro è un'attività comune nella programmazione, utile per gestire appuntamenti, scadenze e varie operazioni temporali. In TypeScript, è possibile utilizzare alcune funzioni built-in per effettuare questi calcoli in modo facile e preciso.

## Come

Per prima cosa, è necessario importare la classe `Date` dalle librerie standard di TypeScript. Con questa classe, è possibile creare un nuovo oggetto `Date` che rappresenta la data corrente. Ad esempio:

```TypeScript
const today = new Date();
```

Per calcolare una data nel futuro, basta utilizzare il metodo `setDate()` dell'oggetto `Date` passando come parametro il numero di giorni desiderati. Per esempio, se vogliamo ottenere la data di 30 giorni dopo quella di oggi, possiamo fare così:

```TypeScript
const futureDate = new Date();
futureDate.setDate(today.getDate() + 30);
```

Per calcolare una data nel passato, invece, basta utilizzare lo stesso metodo `setDate()` ma passando come parametro un valore negativo. Ad esempio, se vogliamo ottenere la data di 30 giorni prima quella di oggi, possiamo fare così:

```TypeScript
const pastDate = new Date();
pastDate.setDate(today.getDate() - 30);
```

Inoltre, è possibile effettuare calcoli più precisi utilizzando i metodi `setFullYear()`, `setMonth()` e `setFullYear()` per impostare rispettivamente l'anno, il mese e il giorno della data.

## Deep Dive

La classe `Date` offre molti altri metodi utili, come ad esempio `getDate()` per ottenere il giorno del mese, `getMonth()` per ottenere il mese (inizia da zero) e `getFullYear()` per ottenere l'anno. Inoltre, è possibile combinare questi metodi con le funzioni di confronto `===` per controllare se una data è maggiore, minore o uguale ad un'altra.

Un'alternativa più avanzata per gestire le date è utilizzare la libreria `moment.js`, che offre una moltitudine di funzioni per effettuare operazioni sulle date in modo ancora più preciso e flessibile.

## Vedi anche

- [Documentazione ufficiale di TypeScript sulla classe Date](https://www.typescriptlang.org/docs/handbook/standard-library.html#working-with-dates)
- [Libreria moment.js](https://momentjs.com/)