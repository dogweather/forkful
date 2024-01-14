---
title:                "TypeScript: Trasformare una data in una stringa."
simple_title:         "Trasformare una data in una stringa."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Il motivo principale per cui uno vorrebbe convertire una data in una stringa è per visualizzare o stampare una data in un formato leggibile per l'utente. Ciò può essere utile quando si costruisce un'applicazione che richiede l'input di una data da parte dell'utente o quando si vuole fornire una data comprensibile nei dati di output.

## Come Fare

```TypeScript
const date = new Date();
const formattedDate = date.toLocaleDateString();

console.log(formattedDate); // Output: 23/02/2021
```

In questa semplice esemplificazione, stiamo creando un oggetto di data JavaScript utilizzando il costruttore `new Date()`. Viene quindi utilizzato il metodo `toLocaleDateString()` per convertire la data in una stringa nell'attuale formato data del sistema. Infine, con `console.log()`, la stringa viene stampata sull'output.

## Approfondimento

La conversione di una data in una stringa può essere fatta utilizzando diversi metodi in TypeScript. Il metodo `toLocaleDateString()` utilizzato nell'esempio sopra accetta due parametri opzionali: `locale` e `options`. Il parametro locale specifica la lingua e la regione per la formattazione della data e può essere impostato su "it-IT" per avere il formato data italiano. Il parametro delle opzioni può essere utilizzato per specificare il formato preciso della data come "short", "medium" o "long".

Oltre al metodo `toLocaleDateString()`, TypeScript offre anche il metodo `toUTCString()` per convertire la data in una stringa nella zona oraria UTC e il metodo `toISOString()` per ottenere una rappresentazione della data seguendo lo standard ISO.

## Vedi Anche
- [Documentation on Date Objects in TypeScript](https://www.typescriptlang.org/docs/handbook/date-and-time.html)
- [MDN Web Docs on Date Objects in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools tutorial on Date Objects in TypeScript](https://www.w3schools.com/js/js_dates.asp)