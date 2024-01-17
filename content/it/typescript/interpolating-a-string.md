---
title:                "Interpolazione di una stringa"
html_title:           "TypeScript: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Interpolare una stringa in TypeScript significa inserire valori dinamici all'interno di una stringa. Questo permette di creare stringhe più flessibili e dinamiche, riducendo la necessità di concatenare manualmente i valori. I programmatori utilizzano l'interpolazione di stringhe per scrivere codice più leggibile, semplice e conciso.

## Come fare:
Ecco un esempio di come utilizzare l'interpolazione di stringhe in TypeScript:

```TypeScript
let nome = "Marco";
let benvenuto = `Ciao ${nome}, benvenuto sul nostro sito!`;
console.log(benvenuto);
```
Output:
`Ciao Marco, benvenuto sul nostro sito!`

Nell'esempio sopra, abbiamo utilizzato le backticks (`` ` ``) per creare una stringa interpolata. All'interno della stringa, abbiamo inserito la variabile `nome` utilizzando la sintassi `${variabile}`. Quando il codice viene eseguito, la variabile viene sostituita con il suo valore all'interno della stringa.

## Approfondimento:
L'interpolazione di stringhe è stata introdotta in TypeScript nella versione 2.4 ed è stata ispirata dalla funzionalità simile presente in ES6. Questo concetto è anche conosciuto come "template literals" e offre un'alternativa più elegante e leggibile rispetto alla concatenazione di stringhe.

Per implementare l'interpolazione di stringhe, il compilatore TypeScript trasforma le stringhe interpolate in una chiamata alla funzione `String.raw`. Questa funzione accetta come argomenti le stringhe e le variabili interpolate, combinandole in una singola stringa.

## Vedi anche:
- [Documentazione ufficiale di TypeScript sull'interpolazione di stringhe](https://www.typescriptlang.org/docs/handbook/strings.html#template-strings)
- [ES6 Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)