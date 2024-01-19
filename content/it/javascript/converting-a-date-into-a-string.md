---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertire una data in una stringa in Javascript

## Cosa & Perché? 

Convertire una data in una stringa significa trasformarla dal tipo di dati data (oggetto date) a una stringa di testo. Lo facciamo per motivi di leggibilità, manipolazione dei dati ed esportazione in altri formati o applicazioni.

## Come fare:

Javascript offre vari metodi per convertire una data in una stringa. Vediamo i più comuni:

```Javascript
let data = new Date(); // crea un oggetto data con la data e l'ora attuali.

// Metodo toDateString():
let stringaData = data.toDateString(); 
console.log(stringaData);   
// Output: "Mon Sep 20 2021"

// Metodo toISOString():
let stringaISO = data.toISOString();
console.log(stringaISO); 
// Output: "2021-09-20T14:56:59.301Z"
```

## Approfondimenti:

- **Context storico**: Javascript ha introdotto i metodi di conversione delle date in ECMAScript 5.1 per facilitare la gestione delle date.

- **Alternative**: Oltre ai metodi nativi di Javascript, esistono librerie esterne come Moment.js che offrono ulteriori funzioni per maneggiare le date.

- **Dettagli di implementazione**: Quando convertiamo una data in una stringa, la data oraria UTC viene presa come riferimento. Questo è importante da considerare se stiamo gestendo fusi orari diversi.

## Vedi anche:

- Documentazione completa su i metodi della data in Javascript: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date

- Moment.js, una libreria di Javascript per la manipolazione delle date: https://momentjs.com/

- ECMAScript 5.1, la specifica che ha introdotto i metodi di conversione delle date: https://www.ecma-international.org/ecma-262/5.1/