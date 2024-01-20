---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La trasformazione di una data da stringa, o parsing, consiste nel leggere un testo (stringa) che rappresenta una data e convertirlo nel formato data utilizzato dai linguaggi di programmazione. I programmatori spesso hanno bisogno di eseguirla quando manipolano dati temporali trasmessi attraverso API, database, o letti tramite input dell'utente.

## Come si fa:

Ecco un esempio di come parsare una data utilizzando TypeScript:

```TypeScript
let dataStringa = "2021-07-06";
let data = new Date(dataStringa);
console.log(data);
```

Questo codice interpreterà la stringa come una data nel formato ISO 8601 e la stampa avrà il seguente output:

```TypeScript
Tue Jul 06 2021 02:00:00 GMT+0200 (Ora legale dell’Europa centrale)
```

## Approfondimento

(1) In termine di contesto storico, il parsing della data è una necessità di lunga data nelle applicazioni software. Molte applicazioni necessitano di convertire stringhe di testo in oggetti data per poter effettuare operazioni come la comparazione, il calcolo della durata, e l'ordinamento.

(2) Esiste un'ampia varietà di metodi alternativi che possono essere usati per parsare date in TypeScript (e JavaScript), come `Date.parse()` o usando librerie come Moment.js.

(3) A livello di implementazione, `new Date(string)` in TypeScript fa affidamento sul metodo `Date.parse()`, il quale da il parsing della stringa fornita asseconda le regole della specifica ECMAScript.

## Guarda anche

Per ulteriori dettagli, consultare le seguenti risorse:

1. [Documentazione ufficiale ECMAScript](https://tc39.es/ecma262/#sec-date.parse)
2. [Libreria Moment.js](https://momentjs.com/)
3. [Formato data ISO 8601 - Wikipedia](https://it.wikipedia.org/wiki/ISO_8601).