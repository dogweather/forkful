---
title:                "Javascript: Calcolare una data nel futuro o nel passato"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare le date nel futuro o nel passato è un'attività comune in programmazione, soprattutto quando si lavora con applicazioni che coinvolgono eventi specifici. Ad esempio, può essere utile sapere quando scade una promozione o quando una prenotazione diventa disponibile.

## Come fare

Per calcolare una data nel futuro o nel passato, esistono diverse opzioni in Javascript. Una delle più comuni è utilizzare la libreria Moment.js, che fornisce una vasta gamma di funzioni per manipolare le date.

Ecco un esempio di come utilizzare Moment.js per ottenere la data di domani:

```Javascript
let domani = moment().add(1, 'days').format('LL');
console.log(domani);
```

In questo esempio, abbiamo utilizzato la funzione `.add()` per aggiungere un giorno alla data corrente, e poi abbiamo utilizzato la funzione `.format()` per formattare la data nel formato "LL", che corrisponde a "MMMM DD, YYYY" (ad esempio, "June 27, 2021").

Ecco un altro esempio di come utilizzare Moment.js per ottenere la data di 3 mesi fa:

```Javascript
let treMesiFa = moment().subtract(3, 'months').format('LL');
console.log(treMesiFa);
```

In questo caso, abbiamo utilizzato la funzione `.subtract()` per sottrarre 3 mesi dalla data corrente.

## Approfondimento

Per calcolare le date nel futuro o nel passato, è importante avere una buona comprensione del concetto di timestamp e di come i computer gestiscono le date. In Javascript, le date sono rappresentate da oggetti di tipo `Date`, che rappresentano i millisecondi trascorsi dal 1° gennaio 1970 a mezzanotte (UTC).

Quando si lavora con timestamp, è importante tenere presente che i calcoli vengono effettuati in base al fuso orario del computer in cui viene eseguito il codice. Quindi, se si sta lavorando con date e orari in più fusi orari, è necessario impostare il fuso orario corretto prima di effettuare i calcoli.

## Vedi anche

- [Documentazione di Moment.js](https://momentjs.com/)
- [Manipolazione delle date con Moment.js](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-in-javascript-using-moment-js)