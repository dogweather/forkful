---
title:                "Converting una data in una stringa"
html_title:           "TypeScript: Converting una data in una stringa"
simple_title:         "Converting una data in una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché convertire una data in una stringa?

Una delle situazioni più comuni in cui si potrebbe dover convertire una data in una stringa è quando si lavora con dati forniti da fonti esterne, come ad esempio un database o un servizio di API. Questi dati possono essere rappresentati in uno specifico formato di data, che potrebbe non essere compatibile con il tuo codice. Convertire la data in una stringa ti permette di manipolarla e utilizzarla come preferisci all'interno del tuo programma.

## Come convertire una data in una stringa

Per convertire una data in una stringa in TypeScript, esistono diverse opzioni a disposizione. Una delle più utilizzate è il metodo `toString()` che permette di convertire una data in una stringa in formato standard ISO.

```TypeScript
let today = new Date();
let dateString = today.toString();
console.log(dateString); // Output: Fri Nov 12 2021 15:30:00 GMT+0100 (Central European Standard Time)
```

Un'altra opzione è utilizzare il metodo `toLocaleString()` che restituisce la data in formato leggibile per l'utente, basandosi sul locale del sistema.

```TypeScript
let today = new Date();
let dateString = today.toLocaleString();
console.log(dateString); // Output: 11/12/2021, 22:30:00
```

Se invece si vuole avere il controllo totale sul formato della stringa, si può utilizzare il metodo `toISOString()` che permette di specificare il formato desiderato utilizzando i parametri opzionali, come ad esempio:

```TypeScript
let today = new Date();
let options = { year: "numeric", month: "long", day: "numeric" };
let dateString = today.toLocaleString("en-US", options);
console.log(dateString); // Output: November 12, 2021
```

## Approfondimento sulla conversione di una data in una stringa

Mentre il metodo `toString()` è il più semplice da utilizzare, è importante notare che restituisce la data in formato standard ISO, che potrebbe non essere facilmente comprensibile per gli utenti. Inoltre, alcune parti della data, come ad esempio il fuso orario, possono variare a seconda dell'interprete JavaScript utilizzato.

L'utilizzo del metodo `toLocaleString()` risolve alcuni di questi problemi, ma è ancora soggetto al locale del sistema, il che potrebbe portare a risultati diversi in base al paese in cui si sta eseguendo il codice.

Infine, il metodo `toISOString()` offre il massimo controllo sul formato della stringa, ma richiede l'uso dei parametri opzionali per ottenere il risultato desiderato. Se si lavora con date provenienti da diverse fonti, è importante assicurarsi che il formato sia sempre gestito in modo uniforme in tutto il codice.

## Vedi anche

- [Documentazione ufficiale su Date in TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#date)
- [Convertire una data in una stringa con i metodi di JavaScript](https://www.w3schools.com/js/js_dates.asp)
- [Formati di data supportati da `toLocaleString()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString#parameters)