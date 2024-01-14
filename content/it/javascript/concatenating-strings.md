---
title:    "Javascript: Unire stringhe"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è una delle funzioni fondamentali della programmazione in Javascript. Consiste nell'unione di due o più stringhe in una sola, ideale per la creazione di testi dinamici. Imparare come concatenare stringhe ti permetterà di migliorare le tue abilità di programmazione e di creare applicazioni più complesse.

## Come fare

La concatenazione di stringhe in Javascript è molto semplice. Puoi farlo utilizzando l'operatore ```+``` o l'operatore di assegnazione ```+=```. Vediamo un esempio:

```Javascript
let nome = "Mario";
let cognome = "Rossi";
let nomeCompleto = nome + " " + cognome;
console.log(nomeCompleto);
```
Output: ```Mario Rossi```

In questo caso, abbiamo creato due variabili, "nome" e "cognome", e le abbiamo poi concatenate utilizzando l'operatore ```+```. È inoltre possibile inserire spazi o altri caratteri tra le stringhe per ottenere il formato desiderato.

## Approfondimento

In Javascript, è possibile concatenare non solo stringhe ma anche altri tipi di dati, come numeri e booleani. Quando si utilizza l'operatore ```+```, è importante fare attenzione al tipo di dati che si sta unendo, in quanto potrebbe portare a risultati inaspettati. Ad esempio:

```Javascript
let num1 = 10;
let num2 = 5;
let somma = num1 + num2;
console.log(somma);
```
Output: ```15```

Ma se utilizziamo l'operatore ```+``` con una stringa, il risultato sarà diverso:

```Javascript
let num1 = 10;
let num2 = "5";
let somma = num1 + num2;
console.log(somma);
```
Output: ```105```

Javascript infatti, quando incontra una stringa, cercherà di convertire i numeri in essa contenuti in stringhe per poterle concatenare.

## Vedi anche

- [Documentazione su stringhe in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Articolo sulla concatenazione di stringhe in Javascript](https://www.w3schools.com/js/js_strings.asp)