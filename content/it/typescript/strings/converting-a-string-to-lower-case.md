---
title:                "Conversione di una stringa in minuscolo"
aliases: - /it/typescript/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:23.394439-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una stringa in minuscolo"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Convertire una stringa in minuscolo significa trasformare tutti i caratteri alfabeticitesti da maiuscolo a minuscolo. I programmatori fanno ciò per uniformare i dati, facilitare confronti case-insensitive e migliorare la ricerca.

## How to:
In TypeScript, usiamo il metodo `.toLowerCase()` per convertire una stringa in minuscolo.

```typescript
let saluto: string = "Ciao Mondo!";
let salutoMinuscolo: string = saluto.toLowerCase();
console.log(salutoMinuscolo); // "ciao mondo!"
```

## Deep Dive
Il metodo `.toLowerCase()` ha una storia lunga tanto quanto i linguaggi di programmazione moderni, ereditato da JavaScript. È essenziale quando si normalizzano i dati per confronti o ricerche testuali.

Alternative:
- Utilizzare `.toLocaleLowerCase()` se hai a che fare con localizzazioni specifiche e culture differenti. 

Implementazione:
- TypeScript si basa su JavaScript, quindi `.toLowerCase()` è implementato come parte del prototipo `String`. JavaScript esegue quest'operazione a livello di Unicode, considerando specifiche regole di mappatura dei caratteri.

## See Also
- [MDN Web Docs: String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [ECMA-262 Specification](https://www.ecma-international.org/ecma-262/)
