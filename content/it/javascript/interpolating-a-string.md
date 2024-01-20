---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolazione di Stringhe in JavaScript

## Che Cosa e Perché?

L'interpolazione di stringhe è un modo per inserire dati variabili direttamente in una stringa. Aiuta i programmatori a mantenere il codice pulito, leggibile e più efficiente.

## Come Fare:

In JavaScript, utilizziamo il "template literal" per interpolare stringhe. È possibile farlo racchiudendo la stringa con il backtick (`) e inserendo le variabili tra `${}`.

```Javascript
let nome = 'Mario';
console.log(`Ciao, ${nome}!`); // Output: "Ciao, Mario!"
```

Oppure, puoi interpolare numeri e operazioni matematiche come segue:

```Javascript
let x = 5;
let y = 10;
console.log(`La somma di x e y è ${x + y}.`); // Output: "La somma di x e y è 15."
```

## Approfondimento

L'interpolazione di stringhe era comune nei linguaggi di programmazione come Perl e Python prima di essere adottata da JavaScript con l'introduzione di ES6. Prima di ES6, i programmatori di JavaScript dovevano concatenare le stringhe utilizzando il '+' o la funzione 'concat', che erano meno efficienti e più difficili da leggere.

Come alternativa, alcuni utilizzano librerie come Lodash per l'interpolazione delle stringhe, sebbene la maggior parte delle volte l'uso dei template literals sia sufficiente.

Per quanto riguarda i dettagli implementativi, JavaScript converte le espressioni tra `${}` in stringhe prima di unirle alla stringa principale. Questo avviene a tempo di esecuzione, consentendo l'interpolazione di espressioni dinamiche.

## Per Saperne di Più

1. [MDN Web Docs: Template literals](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Template_literals)
2. [W3Schools: JavaScript String Interpolation](https://www.w3schools.com/js/js_string_templates.asp)
3. [JavaScript.Info: String Interpolation](https://javascript.info/string#string-interpolation)
4. [ES6 Features: Template Literals](http://es6-features.org/#StringInterpolation)