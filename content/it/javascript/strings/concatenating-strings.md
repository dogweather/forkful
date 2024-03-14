---
date: 2024-01-20 17:35:18.132221-07:00
description: "La concatenazione di stringhe \xE8 la pratica di unire due o pi\xF9\
  \ stringhe di testo in una sola. I programmatori la utilizzano per costruire messaggi,\u2026"
lastmod: '2024-03-13T22:44:43.803681-06:00'
model: gpt-4-1106-preview
summary: "La concatenazione di stringhe \xE8 la pratica di unire due o pi\xF9 stringhe\
  \ di testo in una sola. I programmatori la utilizzano per costruire messaggi,\u2026"
title: Concatenazione di stringhe
---

{{< edit_this_page >}}

## What & Why?
La concatenazione di stringhe è la pratica di unire due o più stringhe di testo in una sola. I programmatori la utilizzano per costruire messaggi, indirizzi web, codice, ecc., in modo dinamico.

## How to:
In JavaScript, si concatena con l'operatore `+` o con i template literals.

```javascript
// Utilizzo dell'operatore +
let saluto = "Ciao, " + "come va?";
console.log(saluto); // "Ciao, come va?"

// Utilizzo dei template literals
let nome = "Giovanni";
let salutoCompleto = `Buongiorno, ${nome}!`;
console.log(salutoCompleto); // "Buongiorno, Giovanni!"
```

## Deep Dive
La concatenazione di stringhe è una funzionalità fondamentale presente fin dalla nascita di JavaScript negli anni '90. In passato, l'operatore `+` era l'unico strumento a disposizione. Ora, i template literals (introdotti in ES6) permettono di incorporare espressioni all'interno delle stringhe, semplificando la sintassi e migliorando la leggibilità.

```javascript
// Esempio di concatenazione multi-linea con ES5
let poesia = "Le foglie morte si raccolgono a mucchi," + 
             "\nle ricordi? Ne parlavamo" + 
             "\ne io passavo vicino a te.";
console.log(poesia);
// Uso dei template literals per multi-linea e espressioni in ES6+
let stagione = "autunno";
poesia = `Le foglie morte si raccolgono a mucchi,
le ricordi? Ne parlavamo
e io passavo vicino a te in questo ${stagione}.`;
console.log(poesia);
```

Gli sviluppatori dovrebbero preferire i template literals per il loro chiaro vantaggio: facilità di lettura, possibilità di multi-linea e capacità di includere espressioni.

## See Also
- [MDN String concatenation](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/First_steps/Strings#string_concatenation)
- [MDN Template literals (Template strings)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
