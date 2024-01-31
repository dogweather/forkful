---
title:                "Ricerca e sostituzione del testo"
date:                  2024-01-20T17:58:21.451993-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ricerca e sostituzione del testo"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Cercare e sostituire testo in JavaScript è fondamentale. Serve per modifica dati dinamici, pulire input, e manipolare stringhe. E' utile in ogni scenario dove il testo deve mutare in modo preciso e veloce.

## How to:
Ecco qualche sapore di JavaScript per la ricerca e la sostituzione di testo:

```javascript
// Sostituire la prima occorrenza di testo
let frase = 'Ciao mondo, mondo!';
let nuovaFrase = frase.replace('mondo', 'pianeta');
console.log(nuovaFrase); // "Ciao pianeta, mondo!"

// Sostituire tutte le occorrenze con l'uso di regex
nuovaFrase = frase.replace(/mondo/g, 'pianeta');
console.log(nuovaFrase); // "Ciao pianeta, pianeta!"
```

## Deep Dive:
La funzione `replace()` è ampiamente usata sin dai primi giorni di JavaScript, introdotta con le funzionalità di base della manipolazione delle stringhe. Utilizza la stringa o espressioni regolari (regex) per matchare e sostituire testo.

Alternative alla `replace()` includono metodi come `split()` e `join()` come soluzione DIY per sostituire tutte le occorrenze di una stringa:

```javascript
let fraseDivisa = frase.split('mondo');   // Dividi la frase
let fraseRiunita = fraseDivisa.join('pianeta'); // Unisci con nuovo testo
console.log(fraseRiunita); // "Ciao pianeta, pianeta!"
```

Mentre `replace()` funziona bene per la maggior parte dei casi, in scenari complessi con pattern, condizioni e sostituzioni elaborate, le regex offrono maggior flessibilità e potenza.

## See Also:
- [MDN Web Docs on String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regex101: un builder interattivo di regex](https://regex101.com/)
- [Eloquent JavaScript: un libro che dedica un intero capitolo alle regex](https://eloquentjavascript.net/09_regexp.html)
