---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "TypeScript: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Calcolare una data nel futuro o nel passato è un'operazione comune per i programmatori, essenziale per sviluppare applicazioni che richiedono la gestione e la manipolazione di date. Questo tipo di calcolo è utilizzato per una varietà di scopi, come ad esempio pianificare eventi, gestire scadenze o semplicemente fornire una migliore esperienza utente.

## Come fare:
Per calcolare una data nel futuro o nel passato in TypeScript, possiamo utilizzare la classe `Date` fornita dalla libreria standard. Possiamo creare un nuovo oggetto `Date` con una data specifica e poi utilizzare i metodi `setDate`, `setMonth` e `setFullYear` per impostare la data e l'ora desiderate.

```
let data = new Date();
data.setDate(data.getDate() + 7);
console.log(data);  // output: Mon Nov 22 2021 15:03:47 GMT+0100 (Central European Standard Time)
```

Se vogliamo calcolare una data specifica nel passato, possiamo utilizzare lo stesso approccio, ma questa volta utilizzando il metodo `setDate` con un numero negativo, invece che positivo.

```
let data = new Date();
data.setMonth(data.getMonth() - 3);
console.log(data);  // output: Wed Jul 21 2021 15:03:47 GMT+0200 (Central European Summer Time)
```

## Approfondimento:
Il calcolo di date è stato un problema complesso per molti anni nella programmazione. In passato, i programmatori utilizzavano librerie esterne o sviluppavano algoritmi personalizzati per affrontare questo problema. Tuttavia, con l'avvento delle librerie standard come quella fornita da TypeScript, il calcolo di date è diventato molto più semplice e veloce.

In alternativa, è possibile utilizzare librerie di terze parti come Moment.js per gestire le date in modo più flessibile e con funzionalità aggiuntive.

## Vedi anche:
- Documentazione ufficiale di TypeScript sulle date: https://www.typescriptlang.org/docs/handbook/dates-and-times.html
- Libreria Moment.js: https://momentjs.com/