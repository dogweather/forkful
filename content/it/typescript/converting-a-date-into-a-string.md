---
title:                "Convertire una data in una stringa"
html_title:           "TypeScript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Convertire una data in una stringa è un processo fondamentale nella programmazione perché consente di rappresentare una data in un formato leggibile per gli esseri umani. Questo rende più semplice la visualizzazione e la comprensione dei dati per gli utenti.

## Come fare:
Per convertire una data in una stringa in TypeScript possiamo utilizzare il metodo ```toDateString()``` o ```toLocaleDateString()```. Ad esempio:

```
let today: Date = new Date();
console.log(today.toDateString()); // Output: Wed May 26 2021
console.log(today.toLocaleDateString()); // Output: 26/05/2021
```

## Approfondimento:
Il concetto di convertire una data in una stringa è stato introdotto per la prima volta nel linguaggio di programmazione C nei primi anni '70. Da allora, molti altri linguaggi hanno adottato questo approccio per rappresentare le date in un formato leggibile per gli utenti. Esistono anche diverse librerie esterne, come moment.js, che offrono metodi aggiuntivi per la conversione delle date in stringhe.

## Vedi anche:
[Documentazione ufficiale di TypeScript sulle date](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)