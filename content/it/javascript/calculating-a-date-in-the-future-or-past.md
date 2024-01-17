---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Javascript: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Calcolare una data nel futuro o nel passato è una funzione molto comune utilizzata dai programmatori. Ci permette di manipolare le date in modo rapido e preciso, rendendo le operazioni temporali più efficienti nei nostri codici.

## Come: 
Un modo semplice per calcolare una data nel futuro o nel passato è utilizzare il metodo ```getDate()``` dell'oggetto ```Date``` in combinazione con il metodo ```setDate()```. Ad esempio, se vogliamo calcolare la data di 7 giorni fa, possiamo utilizzare il seguente codice:

```javascript
let oggi = new Date();
let setteGiorniFa = oggi.getDate() - 7;
oggi.setDate(setteGiorniFa);
console.log(oggi);

```
Output: Fri May 28 2021 00:00:00 GMT+0200 (Central European Summer Time)

Per calcolare una data nel futuro, possiamo semplicemente aggiungere un numero positivo al metodo ```getDate()```. Ad esempio, se vogliamo calcolare la data di una settimana dopo oggi, possiamo utilizzare il seguente codice:

```javascript
let oggi = new Date();
let unaSettimanaDopo = oggi.getDate() + 7;
oggi.setDate(unaSettimanaDopo);
console.log(oggi);

```
Output: Fri Jun 11 2021 00:00:00 GMT+0200 (Central European Summer Time)

## Approfondimento:
Calcolare una data nel futuro o nel passato è diventato molto più facile grazie alla disponibilità di librerie come Moment.js o dayjs. Queste librerie offrono metodi e funzioni avanzati per la manipolazione delle date, rendendo ancora più semplice il nostro lavoro di programmazione.

Inoltre, è importante considerare anche il fuso orario quando si effettuano calcoli di date. JavaScript utilizza il fuso orario del sistema in cui è in esecuzione, quindi potrebbe essere necessario utilizzare metodi come ```getTimezoneOffset()``` per ottenere il fuso orario corretto.

## Vedi anche:
- Documentazione ufficiale di JavaScript: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- Dayjs: https://day.js.org/