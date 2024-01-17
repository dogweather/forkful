---
title:                "Ottenere la data corrente"
html_title:           "Javascript: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Ottenere la data corrente è il processo di ottenere la data e l'ora attuali nel formato desiderato. I programmatori spesso lo fanno per tenere traccia del tempo in cui è stato eseguito un determinato codice o per creare funzionalità basate sul tempo.

## Come fare:

```Javascript
// Esempio 1: Ottenere la data e l'ora nel formato predefinito
let currentDate = new Date();
console.log(currentDate);

// Output: Wed Jun 30 2021 13:45:17 GMT+0200 (Central European Summer Time)

// Esempio 2: Ottenere la data e l'ora nel formato personalizzato
let options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
let currentDate = new Date().toLocaleDateString('it-IT', options);
console.log(currentDate);

// Output: mercoledì, 30 giugno 2021

// Esempio 3: Ottenere solo la data senza l'ora
let currentDate = new Date().toLocaleDateString('it-IT');
console.log(currentDate);

// Output: 30/06/2021
```

## Approfondimento:

Ottenere la data corrente è una funzionalità molto comune nei linguaggi di programmazione e JavaScript non fa eccezione. In passato, non esisteva un modo standard per ottenere la data e l'ora, ma ora grazie agli sviluppi tecnologici e ai nuovi metodi come `new Date()`, è diventato più facile gestire il tempo nei programmi.

Esistono anche altre alternative per ottenere la data corrente, come l'utilizzo di librerie esterne come Moment.js o l'uso di API esterne. Tuttavia, la maggior parte dei programmatori preferisce utilizzare il metodo incorporato `new Date()` per la sua semplicità e praticità.

Per quanto riguarda l'implementazione, il metodo `new Date()` restituisce un oggetto Date contenente la data e l'ora attuali. È possibile utilizzare i metodi incorporati dell'oggetto Date, come `getHours()` e `getMinutes()` per ottenere specifici elementi della data e dell'ora.

## Vedi anche:

- Documentazione ufficiale di JavaScript su `new Date()`: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/