---
title:    "Javascript: Calcolare una data nel futuro o nel passato"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile per molteplici scopi, come ad esempio pianificare eventi o gestire scadenze. Inoltre, è un concetto fondamentale della programmazione che può essere applicato in svariate situazioni.

## Come fare

Per calcolare una data nel futuro o nel passato in JavaScript, è necessario utilizzare il metodo `setDate()` sull'oggetto `Date`. Ecco un esempio di codice che calcola la data di oggi più 7 giorni:

```Javascript 
let oggi = new Date(); // data di oggi
oggi.setDate(oggi.getDate() + 7); // data di oggi + 7 giorni
console.log(oggi);
```

L'output sarà una data nel futuro, espressa in formato data completa (come ad esempio Wed Oct 20 2021).

Per calcolare una data nel passato, basta sostituire il "+" con il "-", come mostrato nel seguente esempio che calcola la data odierna meno 30 giorni:

```Javascript 
let oggi = new Date(); // data di oggi
oggi.setDate(oggi.getDate() - 30); // data di oggi - 30 giorni
console.log(oggi);
```

L'output sarà una data nel passato, sempre espressa in formato data completa.

## Approfondimento

Ora che abbiamo visto come calcolare una data nel futuro o nel passato, è importante comprendere come funziona il metodo `setDate()`.

In JavaScript, le date sono rappresentate dai valori numerici dei millisecondi trascorsi da un punto di riferimento (January 1, 1970). Il valore restituito dal metodo `getDate()` è il numero dei giorni trascorsi dal primo giorno del mese. Pertanto, per ottenere una data nel futuro o nel passato, dobbiamo aggiungere o sottrarre il numero di giorni desiderato dal valore restituito da `getDate()`.

Per ulteriori informazioni sulla gestione delle date in JavaScript, è possibile consultare la documentazione ufficiale su [Date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date) e [setDate()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date/setDate).

## Vedi anche

- [Moment.js](https://momentjs.com/), una libreria JavaScript molto popolare per la gestione delle date
- [Calcolo della data in JavaScript - Un approfondimento](https://www.javascripttutorial.net/javascript-date-add-days/), un articolo che approfondisce la gestione delle date in JavaScript.