---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Le espressioni regolari sono pattern che permettono di trovare corrispondenze nei testi. Vengono usate per il parsing, la validazione di form, la sostituzione di testo e altro, perché sono potenti e flessibili.

## How to:
Ecco come usarle in JavaScript:

```Javascript
// Trovare un numero in una stringa
let testo = "Il 2023 è un grande anno.";
let regex = /\d+/;
console.log(testo.match(regex)); // Output: ['2023']

// Sostituzione di una parola
let saluto = "Ciao mondo";
let nuovoSaluto = saluto.replace("mondo", "universo");
console.log(nuovoSaluto); // Output: "Ciao universo"

// Validazione di un'email
let email = "info@example.com";
let regexEmail = /^[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6}$/;
console.log(regexEmail.test(email)); // Output: true
```

## Deep Dive
Le espressioni regolari sono nate negli anni '50 con l'introduzione di teorie sulla linguistica computazionale. Non esiste solo JavaScript per lavorarci; altri linguaggi come Perl e Python offrono anch'essi potenti strumenti di espressioni regolari. Uno degli aspetti da considerare è la "compilazione" dell'espressione regolare, che in JavaScript avviene in tempo reale se la regex è in una stringa, al contrario se è tra i delimitatori / /, viene compilata quando viene letto lo script.

## See Also
- MDN Web Docs su RegExp: [MDN RegExp](https://developer.mozilla.org/it/docs/Web/JavaScript/Guide/Regular_Expressions)
- Tutorial e test delle espressioni regolari: [Regex101](https://regex101.com/)
- Libro online "Eloquent JavaScript", capitolo sulle espressioni regolari: [Eloquent JavaScript](https://eloquentjavascript.net/09_regexp.html)
