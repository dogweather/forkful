---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Convertire una data in stringa consiste nel trasformare un oggetto Date di TypeScript (o qualsiasi linguaggio di programmazione) in una stringa di testo. I programmatori lo fanno per facilitare la manipolazione dei dati, permettere il confronto tra diverse date e presentare i dati in un formato leggibile agli utenti.

## Come fare:
Ecco un esempio di come convertire una data in stringa in TypeScript:

```TypeScript
let data: Date = new Date(); // Crea un nuovo oggetto Date
let stringaData: string = data.toISOString(); // Converte la data in stringa

console.log(stringaData); // Outputs: 2022-04-29T08:30:26.000Z
```

Questo esempio stampa sul terminale la data corrente in formato ISO 8601.

## Approfondimento:
TypeScript, basato su JavaScript, fornisce vari metodi per convertire una data in stringa. Oltre a `toISOString()`, esistono anche `toDateString()`, `toTimeString()`, `toLocaleDateString()`, `toLocaleTimeString()`, ognuno con un formato di output diverso.

Le basi per la conversione di date in stringhe risalgono agli albori della programmazione. I dati numerici tendono a essere difficili da maneggiare e presentare in una forma che abbia senso per gli umani, e le date non fanno eccezione. Le date come stringhe permettono una manipolazione e presentazione più semplice.

Esistono alternative a questa procedura, come l'uso di librerie esterne o l'implementazione di funzioni personalizzate, ma si tratta di soluzioni più elaborate che non sempre sono necessarie.

TypeScript, come JavaScript, memorizza le date come il numero di millisecondi passati dal 1 gennaio 1970 00:00:00 UTC. Quando si converte un oggetto Date in una stringa, si sta essenzialmente formattando questo numero in un formato più leggibile.

## Vedere Anche:
1. Documentazione ufficiale di TypeScript: [Link](https://www.typescriptlang.org/docs/)
2. Approfondimento sulla classe `Date` in JavaScript (e quindi TypeScript): [Link](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
3. Discussione interessante sulla conversione di date in stringhe: [Link](https://stackoverflow.com/questions/3552461/how-to-format-javascript-date)