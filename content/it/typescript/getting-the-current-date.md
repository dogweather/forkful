---
title:    "TypeScript: Ottenere la data corrente"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La data corrente è un elemento fondamentale in molti aspetti della programmazione. Può essere utilizzata per tenere traccia del tempo, per mostrare informazioni aggiornate agli utenti o per programmare attività future. Inoltre, ottenere la data corrente può aiutare a risolvere problemi che richiedono un'accurata sequenza temporale.

## Come fare

Per ottenere la data corrente in TypeScript, possiamo utilizzare il metodo `new Date()` seguito dal metodo `.toLocaleDateString()` per ottenere una stringa con il formato della data locale. Vediamo un esempio:

\`\`\`TypeScript
const oggi: Date = new Date();
console.log(oggi.toLocaleDateString());
\`\`\`

L'output sarà una stringa contenente la data corrente nel formato locale.

\`\`\`
1/5/2021
\`\`\`

In alternativa, possiamo specificare un formato personalizzato utilizzando il metodo `.toLocaleDateString()` passando come parametro una stringa contenente i simboli per il formato desiderato. Ad esempio:

\`\`\`TypeScript
const oggi: Date = new Date();
const opzioni = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };

console.log(oggi.toLocaleDateString('it-IT', opzioni));
\`\`\`

L'output sarà una stringa contenente la data corrente nel formato specificato.

\`\`\`
martedì 5 gennaio 2021
\`\`\`

## Approfondimento

Ottenere la data corrente non è sempre un'operazione semplice. Ci sono molti aspetti da considerare come il fuso orario, la precisione dei millisecondi e i formati specifici per le diverse lingue. Per esempio, il metodo `.toLocaleDateString()` è influenzato dalle impostazioni di localizzazione del browser, quindi può variare da un dispositivo all'altro. Inoltre, è importante essere consapevoli dei possibili bug e difficoltà legate alla gestione delle date e dei tempi in generale.

## Guarda anche

- [Documentazione di TypeScript su Date](https://www.typescriptlang.org/docs/handbook/javascript-library-integration.html#date)

- [Libreria Moment.js per la gestione delle date in TypeScript](https://momentjs.com/docs/#/displaying/)

- [Articolo della community su Medium "Working with Dates in TypeScript"](https://medium.com/@madasamy/date-in-typescript-8e88a7a2f4b9)