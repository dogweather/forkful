---
title:                "Calcolo della lunghezza di una stringa"
html_title:           "TypeScript: Calcolo della lunghezza di una stringa"
simple_title:         "Calcolo della lunghezza di una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Ottenere la lunghezza di una stringa è un'operazione comune nella programmazione e si riferisce al numero di caratteri presenti in una stringa di testo. I programmatori spesso devono conoscere la lunghezza di una stringa per poter manipolarla o analizzarla in modo efficace.

## Come fare:

```TypeScript
const stringa = "Ciao mondo!";
console.log(stringa.length); 
// Output: 11
```

In questo esempio, la parola "Ciao mondo!" è una stringa e utilizzando il metodo "length" possiamo ottenerne la lunghezza, che in questo caso è 11. Il metodo length è disponibile per tutte le stringhe in TypeScript.

## Approfondimento:

Quando si parla di linguaggi di programmazione, il concetto di stringa è stato introdotto nel 1960 con il linguaggio ALGOL. Dal momento che le stringhe sono una parte importante di qualsiasi programma, la maggior parte dei linguaggi moderni ha implementato un modo per ottenere la loro lunghezza.

Un'alternativa al metodo "length" è l'utilizzo del metodo "charCodeAt (index)", che restituisce il valore Unicode del carattere specificato al determinato indice nella stringa.

## Guarda anche:

- [MDN Web Docs: String.prototype.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [MDN Web Docs: String.prototype.charCodeAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charCodeAt)