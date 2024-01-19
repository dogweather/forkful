---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Concatenare stringhe significa unire due o più stringhe in una sola. Lo facciamo per creare frasi dinamiche, costruire parti di codice HTML, o per manipolare dati di testo in vari modi.

## Come si fa:

Un esempio semplice di concatenazione di stringhe in Javascript:

```Javascript
var saluto = "Ciao, ";
var nome = "Mario!";
var interoSaluto = saluto + nome;

console.log(interoSaluto); // "Ciao, Mario!"
```
In Javascript moderno (ES6), possiamo utilizzare i template string per un processo più pulito ed efficace.

```Javascript
var saluto = "Ciao, ";
var nome = "Mario!";
var interoSaluto = `${saluto}${nome}`;

console.log(interoSaluto); // "Ciao, Mario!"
```

## Approfondimento:

La concatenazione di stringhe è un concetto quasi antico quanto la programmazione stessa. In Javascript, dove la concisione del codice è importante, esistono metodi alternativi come il sopra menzionato template string.

Un'altra alternativa è l'uso del metodo `concat()`. Questo è un po' più verboso, ma altrettanto efficace.

```Javascript
var stringa1 = "Ciao, ";
var stringa2 = "Mario!";
var interoSaluto = stringa1.concat(stringa2);

console.log(interoSaluto); // "Ciao, Mario!"
```

Ricorda, ogni tecnica ha il suo utilizzo ideale. La concatenazione con `+` funziona meglio con poche stringhe, mentre `concat()` e template string sono più ideali per la concatenazione di molte stringhe.

## Vedere anche:

Per approfondire ulteriormente, ecco alcune risorse correlate:

1. [Template strings](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Template_literals) - Una guida approfondita sulle stringhe di modello in Javascript sul sito web Mozilla Developer Network.
2. [Metodo concat()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/concat) - Documento di riferimento MDN sul metodo `concat()`.
3. [Javascript String Concatenation + vs concat()](https://www.codeproject.com/Questions/5263473/Javascript-string-concatenation-vs-concat) - Un'analisi comparativa tra `+` e `concat()`.
4. [Javascript Info: String](https://javascript.info/string) - Una guida generale sulle stringhe in Javascript.