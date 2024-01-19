---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

La conversione di una stringa in minuscolo è una pratica comune in Javascript che cambia ogni lettera maiuscola in una stringa in una lettera minuscola. Questo è particolarmente utile quando si confrontano stringhe, poiché Javascript è sensibile alla maiuscolo/minuscolo.

## Come fare:

Ecco un esempio di come convertire una stringa in minuscolo in Javascript:
  
```Javascript
let str = "Ciao Mondo!";
let lowerStr = str.toLowerCase();
console.log(lowerStr); // output: "ciao mondo!"
```

In questo frammento, usiamo il metodo 'toLowerCase' di Javascript per convertire tutte le lettere maiuscole della stringa 'str' in minuscolo e lo registriamo nella console.

## Un tuffo a fondo:

La conversione di stringhe in minuscolo è stata un elemento chiave nella programmazione fin dagli albori del codice. In Javascript, la conversione di stringhe in minuscolo si basa principalmente sul metodo `toLowerCase()`. Tuttavia, esistono alternative, come il metodo `toLocaleLowerCase()`, che rispetta le regole di maiuscolo/minuscolo specifiche della lingua. 

Le stringhe in Javascript sono immutabili, quindi sia `toLowerCase()` che `toLocaleLowerCase()` restituiscono nuove stringhe. Non modificano la stringa originale.

## Vedi Anche:

Per approfondire, consiglio di consultare le seguenti risorse: 
1. [JavaScript String toLowerCase() Method - W3Schools](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
2. [JavaScript String toLocaleLowerCase() Method - W3Schools](https://www.w3schools.com/jsref/jsref_tolocalelowercase.asp)
3. [String.prototype.toLowerCase() - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
4. [String.prototype.toLocaleLowerCase() - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)