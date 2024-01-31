---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:36:51.343698-07:00
simple_title:         "Estrarre una data da una stringa"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il parsing di una data da una stringa significa estrarre le informazioni temporali e convertirle in un formato utilizzabile dal programma. Gli sviluppatori lo fanno per manipolare, salvare o confrontare date in modo coerente in diverse zone geografiche, sistemi o formati.

## Come fare:
JavaScript fornisce `Date.parse()` ma spesso gestiamo le date con l'oggetto `Date` e i metodi come `new Date()`.

```javascript
// Parsing di una data usando il costruttore Date
let data = new Date('2023-04-01T12:00:00Z');
console.log(data.toString());
// "Sat Apr 01 2023 14:00:00 GMT+0200 (ora legale dell’Europa centrale)"

// Parsing con Date.parse() che restituisce un timestamp
let timestamp = Date.parse('April 1, 2023 12:00:00');
console.log(timestamp);
// 1680416400000
```

## Approfondimento
Nel mondo JavaScript, il parsing delle date ha una storia complicata dovuta a diversi formati e problemi di inconsistenza di browser. Per questo, molte alternative come le librerie `Moment.js` o `date-fns` sono nate per semplificarlo.

Le date JS seguono le specifiche ISO 8601 (`YYYY-MM-DDTHH:mm:ss.sssZ`). Se non specifici un fuso orario, JavaScript usa quello locale. Attenzione agli offset di fuso orario creando date in questo modo.

La libreria `date-fns` fornisce funzioni leggere e modulari per il parsing delle date:

```javascript
// Parsing di una data con date-fns
import { parseISO } from 'date-fns';
let data = parseISO('2023-04-01T12:00:00Z');
console.log(data);
```

Prima della specifica ECMAScript 5, il parsing delle date era ancora più hit or miss, non fidarti del parsing nativo per applicazioni complesse.

## Vedi Anche
- Documentazione MDN su Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- date-fns: https://date-fns.org/
- Specifica ISO 8601: https://www.iso.org/iso-8601-date-and-time-format.html
