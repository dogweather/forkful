---
title:                "Calcolo di una data nel futuro o nel passato"
html_title:           "Javascript: Calcolo di una data nel futuro o nel passato"
simple_title:         "Calcolo di una data nel futuro o nel passato"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Calcolare una data futura o passata significa aggiungere o sottrarre giorni, mesi, anni, ore, minuti, o secondi da una data di partenza. I programmatori lo fanno per implementare funzionalità come countdown, promemoria di eventi futuri, o calcolo della durata tra due date.

## Come si fa:
Ecco come creare una nuova data e aggiungere o sottrarre giorni in Javascript:

```Javascript
let dataOggi = new Date();
console.log("Data di oggi: ", dataOggi);

let dataFutura = new Date();
dataFutura.setDate(dataOggi.getDate() + 5);
console.log("Data futura: ", dataFutura);

let dataPassata = new Date();
dataPassata.setDate(dataOggi.getDate() - 5);
console.log("Data passata: ", dataPassata);
```
Nell'esempio di cui sopra, abbiamo prima definito la data di oggi, poi abbiamo aggiunto 5 giorni per ottenere una data futura e sottratto 5 giorni per ottenere una data passata.

## Approfondimenti:
In Javascript, la gestione delle date può sembrare complicata rispetto ad altri linguaggi come PHP o Python a causa di alcune sue peculiarità. Innanzitutto, Javascript conta i mesi da 0 (gennaio) a 11 (dicembre), e i giorni della settimana da 0 (domenica) a 6 (sabato). Infine, Javascript gestisce le date come millisecondi trascorsi dal 1 Gennaio 1970 (chiamato Epoch Unix), quindi per operare con le date si tratta fundamentalmente di sommare o sottrarre millisecondi.

Un'alternativa alla manipolazione nativa delle date in Javascript sono librerie come Moment.js o Date-fns, che rendono più semplice e leggibile il codice.

## Vedi anche:
Per approfondire l'argomento consulta le risorse ufficiali di Javascript sulle date: [https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)

Se sei interessato a utilizzare librerie esterne, dai un'occhiata a Moment.js [https://momentjs.com](https://momentjs.com) o a Date-fns [https://date-fns.org](https://date-fns.org).