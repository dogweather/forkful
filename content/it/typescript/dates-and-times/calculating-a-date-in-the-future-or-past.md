---
date: 2024-01-20 17:32:29.412439-07:00
description: "Come fare: Storicamente, la gestione delle date in programmazione \xE8\
  \ stata fonte di molti bug e complessit\xE0, per via delle diverse rappresentazioni\
  \ di\u2026"
lastmod: '2024-04-05T21:53:43.962273-06:00'
model: gpt-4-1106-preview
summary: "Storicamente, la gestione delle date in programmazione \xE8 stata fonte\
  \ di molti bug e complessit\xE0, per via delle diverse rappresentazioni di tempo\
  \ (locale, UTC, ecc.), dei fusi orari e delle regole sulle variazioni come le ore\
  \ legali."
title: Calcolo di una data futura o passata
weight: 26
---

## Come fare:
```TypeScript
const calcolaDataFutura = (dataIniziale: Date, giorni: number): Date => {
  const risultato = new Date(dataIniziale);
  risultato.setDate(risultato.getDate() + giorni);
  return risultato;
};

const calcolaDataPassata = (dataIniziale: Date, giorni: number): Date => {
  const risultato = new Date(dataIniziale);
  risultato.setDate(risultato.getDate() - giorni);
  return risultato;
};

// Esempio di utilizzo:
const oggi = new Date();
console.log("Oggi:", oggi);
console.log("Tra 10 giorni:", calcolaDataFutura(oggi, 10));
console.log("10 giorni fa:", calcolaDataPassata(oggi, 10));
```

## Approfondimento:
Storicamente, la gestione delle date in programmazione è stata fonte di molti bug e complessità, per via delle diverse rappresentazioni di tempo (locale, UTC, ecc.), dei fusi orari e delle regole sulle variazioni come le ore legali. In TypeScript, `Date` è un oggetto che rappresenta un singolo momento nel tempo in una maniera indipendente dal fuso orario.

Alternative popolari per gestire date e tempo includono librerie come `moment.js` (ora spesso sostituita da `day.js` per via delle sue dimensioni ridotte) e `date-fns`, che offrono funzionalità più avanzate e gestione semplificata delle problematiche comuni.

In implementazioni più complesse, il calcolo di date nel futuro o nel passato potrebbe richiedere di considerare anche secondi, minuti, ore, e mesi o anni bisestili. Tuttavia, le funzioni native di JavaScript come `getDate`, `setDate`, `getMonth`, `setMonth`, `getFullYear`, e `setFullYear` sono spesso sufficienti per la maggior parte delle esigenze di base.

## Guarda Anche:
- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date): documentazione ufficiale delle API `Date` di JavaScript.
- [date-fns](https://date-fns.org/): una moderna libreria per lavorare con date in JavaScript.
- [Day.js](https://day.js.org/): una libreria lightweight per la manipolazione delle date.
- [ISO 8601](https://it.wikipedia.org/wiki/ISO_8601): lo standard internazionale per la rappresentazione di date e tempi.
