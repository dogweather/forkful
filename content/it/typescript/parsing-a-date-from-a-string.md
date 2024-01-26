---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:38:44.749622-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Il parsing di una data da una stringa significa estrarre e convertire informazioni di data e ora da una rappresentazione testuale. È fondamentale per la manipolazione, il storage e la presentazione di date in formati leggibili e uniformi tra diversi sistemi e linguaggi di programmazione.

## How to:
TypeScript rende semplice il parsing di date con il costruttore `Date` e le librerie di terze parti. Ecco come:

```typescript
// Utilizzo del costruttore Date
const dataStringa = "2023-04-01T14:20:00Z";
const data = new Date(dataStringa);
console.log(data.toString()); // Sat Apr 01 2023 16:20:00 GMT+0200 (Ora legale dell’Europa centrale)

// Parsing con la libreria di terze parti moment.js
import * as moment from 'moment';

const momentData = moment(dataStringa);
console.log(momentData.format("DD/MM/YYYY")); // 01/04/2023
```

## Deep Dive
Storicamente, il parsing delle date nei linguaggi di scripting poteva essere un dolor de capo, specialmente per i formati che non si conformano allo standard ISO 8601 (YYYY-MM-DDTHH:mm:ssZ). TypeScript, essendo un superset di JavaScript, eredita il parsing di date di JS che è migliorato nel tempo.

Alternative per il parsing di date:
- `Date.parse()`: Metodo JavaScript nativo che restituisce il numero di millisecondi da 01/01/1970 UTC.
- Librerie di terze parti come `moment.js`, `date-fns` e `Day.js`: Offrono parsing, validazione, manipolazione e formattazione di date più robuste rispetto ai metodi JavaScript nativi.

Dettagli implementativi:
Quando si usa `new Date(string)`, TypeScript si affida alla implementazione di Date di JavaScript, che può portare a comportamenti diversi a seconda del browser. Con librerie di terze parti, invece, si ottiene consistenza su tutti i browser e ambienti Node.js.

## See Also
- Documentazione di Mozilla Developer Network su Date: [MDN Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js: [Moment.js Docs](https://momentjs.com/docs/)
- Date-fns: [date-fns Docs](https://date-fns.org/)
- Day.js: [Day.js Docs](https://day.js.org/)
