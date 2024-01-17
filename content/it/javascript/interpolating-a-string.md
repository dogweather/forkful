---
title:                "Interpolazione di una stringa"
html_title:           "Javascript: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

L'interpolazione di una stringa è un concetto molto utile nella programmazione di JavaScript. Consiste nell'inserimento di variabili o espressioni all'interno di una stringa, in modo che il risultato finale sia una stringa formattata dinamicamente.

I programmatori spesso utilizzano questa tecnica perché consente loro di creare stringhe personalizzate, basate su dati o condizioni specifiche, senza dover scrivere molteplici linee di codice per modificarle manualmente.

## Come fare:

Ecco un esempio semplice di interpolazione di stringhe in JavaScript:

```
// Dichiarazione di variabili
let name = "Mario";
let age = 22;

// Interpolazione di una stringa con variabili
let intro = `Ciao, mi chiamo ${name} e ho ${age} anni.`;

// Stampa del risultato
console.log(intro);

// Output: Ciao, mi chiamo Mario e ho 22 anni.
```

Un altro esempio comune è l'uso di espressioni all'interno delle stringhe per creare calcoli o confronti dinamici:

```
// Inserimento di un'espressione all'interno di una stringa
let num1 = 10;
let num2 = 5;

let operation = `${num1} + ${num2} = ${num1 + num2}`;

// Stampa del risultato
console.log(operation);

// Output: 10 + 5 = 15
```

## Approfondimento:

L'interpolazione di stringhe è stata introdotta in JavaScript con l'aggiornamento ES6. Prima di questo aggiornamento, i programmatori utilizzavano il metodo "concat" o l'operatore "+" per creare stringhe dinamiche.

Tuttavia, l'interpolazione di stringhe è diventata rapidamente il metodo preferito perché è più leggibile e meno soggetto ad errori. Inoltre, permette l'uso di espressioni e funzioni all'interno delle stringhe, rendendo il codice più conciso.

Come alternative ai template literal, che sono il tipo di stringa utilizzato per l'interpolazione in JavaScript, ci sono anche i template engine come Handlebars o Mustache. Questi consentono di creare stringhe più complesse, inclusi cicli e condizioni, che possono essere utili per la creazione di pagine web dinamiche.

Per quanto riguarda l'implementazione, l'interpolazione di stringhe è stata aggiunta a JavaScript utilizzando l'operatore di backtick ("``"). Quando si utilizza questo operatore prima e dopo la stringa, è possibile inserire variabili o espressioni all'interno utilizzando "${ }".

## Vedi anche:

- [Documentazione di template literal di Mozilla](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Template_literals)
- [Introduzione ai template engine su Freecodecamp](https://www.freecodecamp.org/news/what-are-javascript-template-engines-and-how-to-use-them)