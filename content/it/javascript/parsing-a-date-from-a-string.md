---
title:                "Analisi di una data da una stringa"
html_title:           "Javascript: Analisi di una data da una stringa"
simple_title:         "Analisi di una data da una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Il parsing di una data da una stringa è il processo di estrarre l'informazione della data dalla stringa di testo. I programmatori spesso fanno questo per convertire una stringa di testo contenente una data in un formato comprensibile e manipolabile dal computer.

## How to:

```Javascript
// Metodo 1: Utilizzando il costruttore Date
const stringaData1 = "28 febbraio 2021";
let data1 = new Date(stringaData1);
console.log(data1); // Output: Sun Feb 28 2021 00:00:00 GMT+0100 (Central European Standard Time)

// Metodo 2: Utilizzando il metodo Date.parse()
const stringaData2 = "13-04-2020";
let data2 = Date.parse(stringaData2);
console.log(data2); // Output: 1586742000000 (rappresentante i millisecondi dal 1 gennaio 1970)

// Metodo 3: Utilizzando il metodo Date.UTC()
let data3 = Date.UTC(2020, 7, 30); // Mese indicizzato a partire da 0 (0 = Gennaio, 1 = Febbraio, ecc.)
console.log(data3); // Output: 1598736000000 (rappresentante i millisecondi dal 1 gennaio 1970)
```

## Deep Dive:

Il parsing delle date da una stringa è diventato una necessità comune con la crescente digitalizzazione della società moderna. In passato, le date venivano spesso scritte in formati diversi a seconda del paese o della cultura, il che rendeva difficile per i computer capire l'informazione corretta. Inoltre, con un formato standardizzato della data, i computer possono gestire le date in modo più efficiente e preciso.

Esistono anche altre alternative per il parsing delle date da una stringa, come utilizzare librerie esterne come moment.js o luxon.js. Tali librerie offrono metodi più avanzati per il parsing delle date e possono essere utili quando si lavora con formati della data più complessi.

Per quanto riguarda l'implementazione del parsing delle date da una stringa in Javascript, il metodo più semplice è utilizzare il costruttore Date o il metodo Date.parse(). Entrambi convertiranno la stringa in un oggetto Date, che può poi essere manipolato con vari metodi per ottenere l'informazione desiderata.

## See Also:

- [Documentazione sul parsing delle date in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
- [Moment.js](https://momentjs.com/)
- [Luxon.js](https://moment.github.io/luxon/)