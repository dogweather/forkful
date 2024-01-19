---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

---

## Che Cos'è & Perché? 

Lunghezza stringa è il numero di caratteri in una stringa. I programmatori lo fanno per validare l'input, troncare la stringa, o per eseguire il loop attraverso i caratteri.

## Come se fa:

Ecco come trovare la lunghezza di una stringa in TypeScript:

```TypeScript
let str: string = "Ciao mondo!";
console.log(str.length); // Risultato: 12
```
In questo esempio, la stringa "Ciao mondo!" ha 12 caratteri, comprendendo sia le lettere e lo spazio.

## Sotto il cofano

Historicamente,JavaScript (da cui nasce TypeScript) conta la lunghezza delle stringhe come Unicode code points. Ad esempio, un'emoji come "😀" viene conteggiato come due, non uno.

Invece di utilizzare la proprietà `length`, è anche possibile utilizzare il metodo `split('')` per ottenere un array di tutti i caratteri e poi usare la proprietà perché `length` ritorna la lunghezza dell'array.

```TypeScript
let str: string = "Ciao mondo!";
console.log(str.split('').length); // Risultato: 12
```
Nonostante questo, il modo più frequente ed efficiente è sicuramente usando `length`.

## Vedi Anche

- Documentazione ufficiale di TypeScript sulla [tipizzazione delle stringhe](https://www.typescriptlang.org/docs/handbook/basic-types.html#string).
- Articolo di Mozilla sulla [lunghezza delle stringhe Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/length).