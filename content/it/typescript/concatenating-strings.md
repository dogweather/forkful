---
title:    "TypeScript: Concatenazione di stringhe"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché

La concatenazione delle stringhe è una pratica comune in programmazione che permette di unire più stringhe in una sola. Questa operazione è particolarmente utile quando si vuole creare un testo dinamico o comporre un messaggio personalizzato.

## Come Fare

Per concatenare le stringhe in TypeScript, si possono utilizzare due metodi: l'operatore "+" o il metodo "concat()". Di seguito un esempio di entrambi i metodi utilizzando due stringhe contenenti il nome e il cognome di una persona:

```TypeScript
let nome: string = "Marco";
let cognome: string = "Rossi";

// utilizzando l'operatore +
let nomeCompleto: string = nome + " " + cognome;
console.log(nomeCompleto); // output: Marco Rossi

// utilizzando il metodo concat()
let nomeCompleto2: string = nome.concat(" ", cognome);
console.log(nomeCompleto2); // output: Marco Rossi
```

## Approfondimento

Vediamo ora alcuni casi particolari della concatenazione delle stringhe:

- Se una delle stringhe da concatenare è vuota, il risultato sarà la stringa non vuota. Ad esempio: "hello" + "" = "hello".
- Se entrambe le stringhe sono vuote, il risultato sarà una stringa vuota. Ad esempio: "" + "" = "".

## Vedi Anche

- [Documentazione ufficiale di TypeScript sulla concatenazione delle stringhe](https://www.typescriptlang.org/docs/handbook/2/objects.html#string-concatenation)
- [Articolo su come concatenare le stringhe in JavaScript](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Guida all'utilizzo dei template literal in TypeScript](https://medium.com/@nitinpatel_20236/typescript-string-operations-template-literals-and-interpolation-85c3e949e623)