---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:15:12.786870-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Ottenere la data corrente in JavaScript significa semplicemente avere l'accesso alle informazioni sul giorno, mese, anno, ore e minuti in tempo reale. I programmatori lo fanno per funzionalità come calendari, orologi, e per registrare il momento esatto in cui accadono eventi all'interno delle loro applicazioni.

## Come fare:
```Javascript
// Ottenere la data corrente
const oggi = new Date();

// Stampare la data completa
console.log(oggi.toString()); // Tue Mar 14 2023 15:20:30 GMT+0100 (Standard europeo centrale)

// Stampare in formato YYYY-MM-DD
console.log(oggi.toISOString().split('T')[0]); // 2023-03-14

// Ottenere componenti specifici della data
console.log(`Anno: ${oggi.getFullYear()}`); // Anno: 2023
console.log(`Mese: ${oggi.getMonth() + 1}`); // Mese: 3, (+1 perché i mesi partono da 0)
console.log(`Giorno: ${oggi.getDate()}`); // Giorno: 14
console.log(`Ora: ${oggi.getHours()}`); // Ora: 15
console.log(`Minuti: ${oggi.getMinutes()}`); // Minuti: 20
```

## Approfondimenti
JavaScript fornisce l'oggetto `Date` dalla sua nascita, parte della specifica ECMAScript. Prima di questi strumenti incorporati, calcolare le date era più laborioso e soggetto a errori. Altri modi per manipolare le date includono librerie come Moment.js, ma queste stanno diventando sempre meno utilizzate con l'arricchimento delle funzioni native del linguaggio. Riguardo all'implementazione, `Date` usa valori numerici per mesi da 0 (gennaio) a 11 (dicembre), il che può confondere i neofiti. Anche i fusi orari sono automaticamente gestiti, ma possono richiedere attenzione per essere manipolati correttamente. 

## Vedi anche
- MDN Web Docs su `Date`: [MDN Date](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Documentazione ECMAScript (specifica ufficiale del linguaggio): [ECMAScript](https://www.ecma-international.org/)
- Moment.js, una libreria di terze parti per lavorare con le date: [Moment.js](https://momentjs.com/)
