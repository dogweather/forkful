---
title:                "TypeScript: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato è un'operazione molto comune nella programmazione. Può essere utile per pianificare eventi, gestire scadenze o impostare promemoria.

## Come fare

Per calcolare una data nel futuro o nel passato in TypeScript, possiamo utilizzare la classe *Date* e i suoi metodi *setDate* e *getDate*.

````TypeScript
let currentDate = new Date(); // Data odierna

// Per calcolare una data nel futuro utilizziamo il metodo setDate con un valore positivo
currentDate.setDate(currentDate.getDate() + 7); // Data tra 7 giorni

// Per calcolare una data nel passato utilizziamo il metodo setDate con un valore negativo
currentDate.setDate(currentDate.getDate() - 3); // Data 3 giorni fa

console.log(currentDate); // Output: 2021-08-28T00:00:00.000Z
````

In questo esempio, abbiamo creato una variabile *currentDate* che contiene la data odierna. Utilizzando il metodo *setDate* con un valore positivo o negativo, possiamo aggiungere o sottrarre un determinato numero di giorni alla data corrente.

## Approfondimento

In aggiunta ai metodi *setDate* e *getDate*, la classe *Date* offre una serie di altre opzioni utili per calcolare date nel futuro o nel passato, come ad esempio *setMonth*, *setFullYear* e *setTime*.

Inoltre, esistono delle librerie esterne come *date-fns* o *moment.js* che offrono funzionalità avanzate per la gestione delle date in TypeScript.

## Vedi anche

- [Documentazione ufficiale di Date in TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#support-for-symbol-dynamic-being-set-to-undefined)
- [Libreria date-fns in TypeScript](https://www.npmjs.com/package/date-fns)
- [Libreria moment.js in TypeScript](https://momentjs.com/docs/#/parsing)