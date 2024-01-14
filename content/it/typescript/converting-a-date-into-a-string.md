---
title:                "TypeScript: Convertire una data in una stringa"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

In TypeScript, la conversione di una data in una stringa è un'operazione comune quando si lavora con dati temporali. Questo permette di visualizzare in modo leggibile e comprensibile le date all'interno della nostra applicazione.

## Come Fare

Per convertire una data in una stringa in TypeScript, è possibile utilizzare il metodo `toString()` dell'oggetto `Date`. Di seguito è riportato un esempio di codice che mostra come utilizzare questo metodo:

```TypeScript
let myDate = new Date();
console.log(myDate.toString());
```

Questo codice creerà un nuovo oggetto `Date` che rappresenta la data e l'ora corrente e lo convertirà in una stringa, che verrà quindi stampata sulla console. Il risultato dovrebbe essere qualcosa del genere: "Fri Oct 22 2021 09:30:00 GMT+0200 (Central European Summer Time)".

## Approfondimento

Se si vuole controllare il formato della stringa generata, è possibile utilizzare il metodo `toLocaleString()` invece di `toString()`. In questo modo, è possibile specificare il formato della data e dell'ora in base alla lingua e alle preferenze locali dell'utente. Ad esempio:

```TypeScript
let myDate = new Date();
console.log(myDate.toLocaleString("it-IT")); // output: "22/10/2021 09:30:00"
```

Inoltre, TypeScript fornisce la libreria di utility `DatePipe` che può essere utilizzata per formattare le date in modo più approfondito. Questa libreria offre una maggiore flessibilità nel formato della stringa generata, permettendo di personalizzare ulteriormente la visualizzazione della data.

## Vedi Anche

- [Documentazione ufficiale di TypeScript su Date](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)
- [Tutorial su come lavorare con le date in TypeScript](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-times-in-typescript)