---
title:                "Javascript: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perchè

Calcolare una data nel futuro o nel passato può essere utile in diverse situazioni, ad esempio per programmare un evento o per calcolare l'età di qualcuno.

## Come Fare

La logica di base per calcolare una data nel futuro o nel passato è la stessa: bisogna prima ottenere la data attuale e poi aggiungere o sottrarre il numero di giorni, mesi o anni desiderato. Con Javascript, è possibile utilizzare l'oggetto Date e i suoi metodi per ottenere e manipolare date.

```
// Ottenere la data attuale
let today = new Date();

// Aggiungere 7 giorni alla data attuale
let futureDate = today.setDate(today.getDate() + 7);

// Stampare il risultato in formato MM/GG/AAAA
console.log(`${today.getMonth() + 1} / ${today.getDate()} / ${today.getFullYear()}`);
// Output: 3 / 23 / 2020
```

Nell'esempio sopra, abbiamo utilizzato il metodo `setDate()` per impostare il giorno della data al valore della data attuale più 7 giorni. È importante notare che i mesi in Javascript vanno da 0 a 11, quindi abbiamo dovuto aggiungere 1 quando abbiamo stampato il mese.

È possibile utilizzare lo stesso approccio per sottrarre una quantità di giorni, mesi o anni dalla data attuale. Ad esempio, per ottenere la data di oggi 2 anni fa si potrebbe utilizzare il seguente codice:

```
let twoYearsAgo = today.setFullYear(today.getFullYear() - 2);
console.log(`${today.getMonth() + 1} / ${today.getDate()} / ${today.getFullYear()}`);
// Output: 3 / 23 / 2018
```

## Approfondimento

L'oggetto Date in Javascript può sembrare complicato, ma è molto potente e versatile. Oltre ai metodi utilizzati nell'esempio sopra, ci sono molti altri che possono essere utili per calcolare date in modo più preciso. Ad esempio, il metodo `setMonth()` permette di impostare il mese di una data specifica.

Un altro modo per calcolare date nel futuro o nel passato è utilizzando la libreria Moment.js, che offre funzioni più avanzate e una sintassi più semplice per manipolare date e orari.

## Vedi Anche

- [Documentazione ufficiale di Javascript Date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/) – libreria per la manipolazione delle date in Javascript