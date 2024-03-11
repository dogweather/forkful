---
date: 2024-01-20 17:47:37.005289-07:00
description: "Misurare la lunghezza di una stringa significa contare i caratteri che\
  \ la compongono. I programmatori lo fanno per validare input, manipolare testi,\
  \ o\u2026"
lastmod: '2024-03-11T00:14:17.420155-06:00'
model: gpt-4-1106-preview
summary: "Misurare la lunghezza di una stringa significa contare i caratteri che la\
  \ compongono. I programmatori lo fanno per validare input, manipolare testi, o\u2026"
title: Trovare la lunghezza di una stringa
---

{{< edit_this_page >}}

## What & Why?
Misurare la lunghezza di una stringa significa contare i caratteri che la compongono. I programmatori lo fanno per validare input, manipolare testi, o semplicemente per sapere "quanto è lungo".

## How to:
Usiamo la proprietà `length` di una stringa. Ecco come:

```javascript
let saluto = "Ciao!";
console.log(saluto.length); // Output: 5

let frase = "Buongiorno, come va?";
console.log(frase.length); // Output: 21
```

## Deep Dive
La proprietà `length` esiste da quando le stringhe sono state introdotte in JavaScript. Prima del metodo `length`, si doveva creare cicli per contare i caratteri. Ci sono alternative, come il metodo `split().length`, ma non c'è motivo di non usare `length`.

```javascript
// Alternativa con split()
let saluto = "Ciao!";
console.log(saluto.split('').length); // Output: 5
```

Ma attenzione: `length` non tiene conto di caratteri speciali come emojior caratteri unicode composti. Per questi casi, potrebbe servire un approccio diverso, che è al di là degli scopi di quest'articolo.

## See Also
- MDN Web Docs per una panoramica completa su stringhe e `length`: [MDN String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- Una discussione su Stack Overflow riguardo il conteggio dei caratteri Unicode: [Stack Overflow Unicode Counting](https://stackoverflow.com/questions/5436951/javascript-string-length-difference-between-s-len-and-split-length)
