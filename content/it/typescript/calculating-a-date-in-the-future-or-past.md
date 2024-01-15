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

## Perché

Calcolare una data nel futuro o nel passato può essere utile per una vasta gamma di applicazioni, sia nella vita quotidiana che nella programmazione. Ad esempio, si potrebbe voler calcolare una data di scadenza per un progetto o un'appuntamento importante, o trovare il giorno della settimana in cui è nato un amico.

## Come Fare

Per calcolare una data nel futuro o nel passato in TypeScript, possiamo utilizzare la libreria di date integrata di JavaScript. Questa libreria ci fornisce una serie di metodi utili per manipolare le date.

```TypeScript

// Calcolare una data nel futuro

let today = new Date(); // ottieni la data odierna
let oneWeekFromNow = new Date(today.getTime() + (7 * 24 * 60 * 60 * 1000)); // aggiungi una settimana alla data odierna
console.log(oneWeekFromNow.toLocaleDateString()); //  stampa la data in formato locale

// Output: "11/06/2021" (supponendo che oggi sia il 04/06/2021)

// Calcolare una data nel passato

let dateOfBirth = new Date(1990, 5, 23); // creare una nuova data con anno, mese e giorno specifici
let daysAgo = new Date(today.getTime() - dateOfBirth.getTime()); // calcola la differenza tra la data corrente e la data di nascita
console.log(daysAgo.getDay()); // stampa il giorno della settimana in cui è nato (0 = Domenica, 1 = Lunedì, ecc.)

// Output: 4 (supponendo che oggi sia il Venerdì e che la data di nascita sia un Martedì)

```

## Approfondimento

Per calcolare date più complesse, possiamo utilizzare metodi come `setFullYear()`, `setMonth()`, `setDate()` per impostare manualmente gli anni, i mesi e i giorni delle date. Possiamo anche utilizzare metodi come `getTimezoneOffset()` per gestire fusi orari diversi.

## Vedi Anche

- [Documentazione di TypeScript sulle Date](https://www.typescriptlang.org/docs/handbook/datetime.html)
- [Manipolazione di Date in JavaScript](https://www.w3schools.com/js/js_dates.asp)
- [Introduzione a TypeScript](https://www.typescriptlang.org/docs/)