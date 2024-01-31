---
title:                "Estrazione di sottostringhe"
date:                  2024-01-20T17:46:41.484275-07:00
model:                 gpt-4-1106-preview
simple_title:         "Estrazione di sottostringhe"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Estrarre le sottostringhe significa selezionare parti specifiche di una stringa. Lo facciamo per validazione, manipolazione dei dati, o semplicemente per ottenerne una porzione rilevante.

## How to:
```typescript
let str: string = "Ciao, mondo!";

// Metodo slice()
let substr: string = str.slice(0, 4); // "Ciao"
console.log(substr);

// Metodo substring()
substr = str.substring(5, 11); // "mondo!"
console.log(substr);

// Metodo substr() - deprecato in JavaScript ma ancora visto in codici vecchi
substr = str.substr(7, 5); // "ondo!"
console.log(substr);
```

## Deep Dive
In TypeScript, le sottostringhe si afferrano facilmente grazie ai metodi integrati di JavaScript. Nella storia, abbiamo visto `substr()`, `substring()`, e `slice()`. `substr()` oggi è deprecato, quindi suggeriamo `slice()` o `substring()`.

`slice()` è potente, con supporto per indici negativi, interpretati dall'estremità della stringa. `substring()` invece non accetta negativi e, se passato un indice più grande, lo scambia con il più piccolo, garantendo coerenza nell'indice di inizio e fine.

L'estrazione di sottostringhe in TypeScript non si discosta da JavaScript, essendo TypeScript un superset tipizzato. Questo dettaglio semplifica la transizione per sviluppatori JavaScript che iniziano ad usare TypeScript.

## See Also
- MDN Web Docs, String methods: [String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice), [String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- TypeScript Handbook: [Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
