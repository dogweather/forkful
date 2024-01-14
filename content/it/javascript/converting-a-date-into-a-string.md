---
title:                "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Convertire una data in una stringa può sembrare un'operazione banale, ma in realtà è un'operazione comune e utile nella programmazione. È possibile utilizzare questa funzionalità per visualizzare la data in un formato specifico, come ad esempio "27 Marzo 2021" anziché "2021-03-27".

## Come Fare
Per convertire una data in una stringa in Javascript, possiamo utilizzare il metodo `toString ()` dell'oggetto `Date`. Ecco un esempio di codice:

```Javascript
let data = new Date();
let dataStringa = data.toString();
console.log(dataStringa); // Output: Sab Mar 27 2021 16:30:00 GMT+0100 (Ora standard dell'Europa centrale)
```

È anche possibile specificare un formato personalizzato utilizzando i metodi `getDate()`, `getMonth()` e `getFullYear()` combinati con stringhe di testo. Di seguito un esempio:

```Javascript
let data = new Date();
let giorno = data.getDate();
let mese = data.getMonth() + 1; // Mese viene rappresentato con numeri da 0 a 11, quindi aggiungiamo 1 per ottenerne il valore corretto
let anno = data.getFullYear();
let dataStringa = giorno + " " + mese + " " + anno;
console.log(dataStringa); // Output: 27 3 2021
```

## Approfondimento
Nel linguaggio Javascript, le date sono rappresentate come oggetti e possiamo accedere ai suoi diversi componenti (giorno, mese, anno, ecc.) tramite i metodi `getDate()`, `getMonth()`, `getFullYear()` e così via. Quando utilizziamo il metodo `toString()` stiamo effettivamente convertendo l'oggetto data in una stringa leggibile per l'utente.

Inoltre, possiamo utilizzare librerie esterne come Moment.js per gestire le date in modo più efficiente e fornire maggiori opzioni di formattazione.

## Vedi Anche
- [La documentazione di MDN su Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js - Libreria per la gestione delle date in Javascript](https://momentjs.com/)