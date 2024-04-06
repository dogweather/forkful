---
date: 2024-01-20 17:35:52.736175-07:00
description: "Come fare: Utilizzando i template literals per una sintassi pi\xF9 pulita."
lastmod: '2024-04-05T21:53:43.942203-06:00'
model: gpt-4-1106-preview
summary: "Utilizzando i template literals per una sintassi pi\xF9 pulita."
title: Concatenazione di stringhe
weight: 3
---

## Come fare:
```TypeScript
let saluto = "Ciao";
let nome = "Luca";
let messaggio = saluto + ", " + nome + "!";
console.log(messaggio); // Output: "Ciao, Luca!"
```

Utilizzando i template literals per una sintassi più pulita:

```TypeScript
let saluto = "Ciao";
let nome = "Luca";
let messaggio = `${saluto}, ${nome}!`;
console.log(messaggio); // Output: "Ciao, Luca!"
```

## Approfondimento
La concatenazione di stringhe esiste da quando sono stati inventati i primi linguaggi di programmazione. Nel tempo, i metodi per concatenare sono evoluti. In TypeScript, oltre all'operatore `+`, possiamo usare i template literals (introdotti in ES6), che rendono il codice più leggibile e riducono la possibilità di errori.

Alternative alla concatenazione standard includono il metodo `concat()`:

```TypeScript
let messaggio = saluto.concat(", ", nome, "!");
console.log(messaggio); // Output: "Ciao, Luca!"
```

Inoltre, ci sono funzioni come `join()` per gli array, utile se hai una lista di stringhe da unire:

```TypeScript
let parole = ["Ciao", "Luca", "!"];
let messaggio = parole.join(" ");
console.log(messaggio); // Output: "Ciao Luca !"
```

Dal punto di vista dell'implementazione, bisogna considerare l'efficienza, soprattutto quando si concatenano grandi quantità di stringhe. I template literals e gli operatori `+` sono generalmente ottimizzati dai moderni motori JavaScript, ma in alcuni contesti le prestazioni possono variare.

## Vedi anche:
- [Template Literals su MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [String.prototype.concat() su MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Array.prototype.join() su MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)
