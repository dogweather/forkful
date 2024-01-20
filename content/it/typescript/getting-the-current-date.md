---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:16:49.503319-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente è come estrarre la data e l'ora presente. I programmatori lo usano per tracciare eventi, misurare il tempo trascorso o fornire timestamp.

## How to:
```TypeScript
const dataAttuale = new Date();
console.log(dataAttuale);
```
Output:
```
2023-03-28T15:34:07.000Z
```
Per ottenere solo la data in un formato più leggibile:
```TypeScript
const opzioni = { year: 'numeric', month: 'long', day: 'numeric' };
console.log(dataAttuale.toLocaleDateString('it-IT', opzioni));
```
Output:
```
28 marzo 2023
```

## Deep Dive
TypeScript, essendo un superset di JavaScript, gestisce le date come JavaScript. La `Date` API è presente da quando JavaScript ha fatto la sua prima apparizione nel 1995. Anche se ci sono biblioteche alternative, come `moment.js` o `date-fns`, la classe `Date` integrata è ampiamente usata per la sua semplicità ed è supportata in tutti i motori moderni di JS.

In TypeScript, l'uso di `new Date()` è fortemente tipizzato, ma la sua implementazione è ereditata direttamente da JavaScript. Ci sono alcune sottigliezze: ad esempio, l'oggetto `Date` in JavaScript è basato sul tempo Unix, che conta i millisecondi da mezzanotte del 1 gennaio 1970, UTC.

Devi anche tener conto del timezone. Usando `toLocaleDateString`, puoi formatizzare le date nel formato locale, che è particolarmente utile in ambito internazionale come la programmazione web, assicurandoti che gli utenti vedano le date nel formato a loro più familiare.

## See Also
- MDN Web Docs per `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Documentazione di `date-fns`: https://date-fns.org/
- Moment.js per alternative: https://momentjs.com/