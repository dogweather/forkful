---
title:                "Analisi di una data da una stringa"
html_title:           "TypeScript: Analisi di una data da una stringa"
simple_title:         "Analisi di una data da una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il parsing di una data da una stringa è il processo di conversione di una data espressa come stringa in un formato di data comprensibile per il computer. I programmatori lo fanno perché spesso devono manipolare e calcolare con le date all'interno del loro codice.

## Come fare:
Ecco un esempio di come effettuare il parsing di una data da una stringa in TypeScript:

```TypeScript
const stringData = '11/09/2021';
const parsedDate = new Date(stringData);
console.log(parsedDate);
```
Output:
```TypeScript
2021-11-09T00:00:00.000Z
```

Il codice usa la classe Date di TypeScript per creare un nuovo oggetto data a partire dalla stringa fornita. In questo caso, il risultato è una data nel formato ISO 8601. È possibile ottenere una data in un formato diverso utilizzando i metodi della classe Date, come ad esempio `toDateString()` o `toISOString()`.

## Approfondimento:
Il parsing di una data da una stringa può essere un'operazione complessa a causa della varietà di formati di data utilizzati in tutto il mondo. In passato, i programmatori dovevano scrivere il proprio codice per gestire queste differenze, ma ora la maggior parte dei linguaggi di programmazione, compreso TypeScript, fornisce strumenti per facilitare questo processo.

Un'alternativa al parsing di una data da una stringa è l'utilizzo di librerie di terze parti come Moment.js, che semplificano la gestione delle date e dei loro formati. Tuttavia, queste librerie possono aggiungere un sovraccarico al codice e alcune di esse sono state dichiarate obsolete poiché le funzionalità sono ora supportate direttamente dai linguaggi di programmazione.

Per quanto riguarda l'implementazione di TypeScript, la classe Date utilizza una matrice di traduzione interna per gestire i diversi formati di input delle date. Inoltre, TypeScript supporta anche l'utilizzo della libreria standard di JavaScript, chiamata moment.js, che offre diverse funzionalità avanzate per la manipolazione delle date.

## Vedi anche:
- [Documentazione ufficiale di TypeScript sulla classe Date](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-7.html#more-accurate-constructors-for-date)
- [Moment.js](https://momentjs.com/)