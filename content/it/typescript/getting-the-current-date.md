---
title:                "Ottenere la data corrente"
html_title:           "TypeScript: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Ottenere la data corrente è il processo di ricevere la data attuale nel formato specifico desiderato. I programmatori spesso utilizzano questo per scopi di programmazione come la registrazione del tempo di un evento o la gestione dei timestamp per le loro applicazioni.

## Come fare:

Ecco un esempio di come ottenere la data corrente in TypeScript utilizzando l'oggetto Date di JavaScript:

```TypeScript
let currentDate = new Date();
console.log(currentDate); // output: Wed Aug 18 2021 17:01:52 GMT+0200 (Central European Summer Time)
```

Possiamo anche formattare la data per visualizzarla in un formato specifico utilizzando i metodi dell'oggetto Date:

```TypeScript
let currentDate = new Date();
let day = currentDate.getDate();
let month = currentDate.getMonth() + 1;
let year = currentDate.getFullYear();
console.log(`${day}/${month}/${year}`); // output: 18/8/2021
```

## Immersione profonda:

La necessità di ottenere la data corrente risale ai primi giorni della programmazione. Prima dell'introduzione dell'oggetto Date in JavaScript nel 1995, i programmatori dovevano utilizzare funzioni di sistema del sistema operativo per accedere alla data corrente. Oltre all'oggetto Date, uno degli strumenti più utilizzati per ottenere la data corrente è Moment.js, una libreria JavaScript popolare per la manipolazione delle date.

Implementare la funzionalità di ottenere la data corrente in TypeScript è semplice grazie alla sua familiarità con JavaScript e alla disponibilità dell'oggetto Date. Tuttavia, è importante prestare attenzione ai fusi orari e agli aggiornamenti automatici della data per garantire una corretta gestione della data corrente all'interno dell'applicazione.

## Vedi anche:

- [Documentazione ufficiale di Date in TypeScript](https://www.typescriptlang.org/docs/handbook/date-and-time.html)
- [Moment.js](https://momentjs.com/)