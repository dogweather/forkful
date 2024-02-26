---
date: 2024-01-20 17:51:00.255099-07:00
description: "L\u2019interpolazione di stringhe in JavaScript consente di inserire\
  \ espressioni all\u2019interno di una stringa, rendendone la composizione dinamica\
  \ e flessibile.\u2026"
lastmod: '2024-02-25T18:49:41.647105-07:00'
model: gpt-4-1106-preview
summary: "L\u2019interpolazione di stringhe in JavaScript consente di inserire espressioni\
  \ all\u2019interno di una stringa, rendendone la composizione dinamica e flessibile.\u2026"
title: Interpolazione di una stringa
---

{{< edit_this_page >}}

## Cosa & Perché?
L’interpolazione di stringhe in JavaScript consente di inserire espressioni all’interno di una stringa, rendendone la composizione dinamica e flessibile. È utile perché semplifica la concatenazione di stringhe e variabili, migliorando la leggibilità del codice.

## Come si fa:
```Javascript
// Template strings con backticks
let nome = 'Luca';
let saluto = `Ciao, ${nome}! Come stai?`;
console.log(saluto); // Output: Ciao, Luca! Come stai?

// Concatenazione tradizionale con il "+"
let nome = 'Sofia';
let saluto = 'Ciao, ' + nome + '! Come stai?';
console.log(saluto); // Output: Ciao, Sofia! Come stai?
```

## Approfondimento
Nel passato, JavaScript richiedeva l'uso dell'operatore `+` per unire le stringhe, il che poteva diventare ingombrante e confusionario. Con l'ES6, introdotto nel 2015, sono state aggiunte le template strings, che usano il carattere backtick (\`) e permettono l'interpolazione con la sintassi `${expression}`. Questo metodo non solo è più espressivo ma aiuta a prevenire errori comuni di concatenazione. Inoltre, rispetto a metodi precedenti, l’interpolazione rende più semplice inserire valori dinamici e gestire multilinea automaticamente, senza bisogno di operatori o metodi aggiuntivi.

## Vedi Anche
- MDN Web Docs sulle [template literals](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Template_literals)
- Articolo su [ES6 Template Literals](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Template_literals)
