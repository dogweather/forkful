---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
In TypeScript, le *regular expressions* (dette anche regex) sono strumenti per cercare e manipolare stringhe di testo. I programmatori le usano perché sono potenti e efficienti nel trovare pattern specifici, validare input, o fare sostituzioni complesse in meno tempo.

## How to:
```typescript
// Esempio di ricerca di una parola in una frase
const frase = "Ciao mondo!";
const regex = /mondo/;
console.log(regex.test(frase)); // Output: true

// Esempio di sostituzione di testo
const testo = "TypeScript è fantastico!";
const nuovoTesto = testo.replace(/fantastico/, 'incredibile');
console.log(nuovoTesto); // Output: TypeScript è incredibile!

// Esempio di estrazione di numeri da una stringa
const info = "L'ordine numero è 12345";
const regexNumeri = /\d+/g;
const numeri = info.match(regexNumeri);
console.log(numeri); // Output: ['12345']
```
## Deep Dive:
Le regular expressions esistono da decenni, con le loro radici negli automi e nella teoria formale dei linguaggi che sono la base per la ricerca e manipolazione di testo nei computer. Alternativamente, si possono usare metodi come `indexOf`, `startsWith`, `endsWith` per ricerche semplici ma le regex offrono molta più flessibilità. In TypeScript, la classe `RegExp` e i relativi metodi forniscono implementazioni native di regular expressions con performance ottimizzate.

## See Also:
- MDN Web Docs su RegExp: [MDN RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- TypeScript Handbook – Regular Expressions: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/2/objects.html#regex-property-names)
- Regular expressions in practice: [RegexOne](https://regexone.com/)
- Esercitazione Regex interattiva: [Regex101](https://regex101.com/)
