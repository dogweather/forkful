---
title:                "Javascript: Stampa dell'output di debug"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

##Perché:

Stampe di output di debug sono uno strumento utile per capire cosa sta accadendo all'interno del nostro codice. Ci permettono di visualizzare i valori di variabili e controllare se le nostre condizioni e loop funzionano come previsto. Quando affrontiamo problemi di codifica, stampare l'output di debug può aiutarci a risolvere rapidamente il problema.

##Come Fare:

Per stampare un output di debug in Javascript, possiamo utilizzare il comando `console.log()`. Questo ci permette di visualizzare un messaggio o il valore di una variabile all'interno della nostra console di sviluppo. Ad esempio:

```Javascript
var num1 = 5;
var num2 = 10;
console.log(num1 + num2);
```
Questo esempio stampa il valore `15` all'interno della nostra console, il che ci permette di verificare che la somma dei due numeri sia corretta.

##Approfondimento:

L'uso di stampe di output di debug non è solo utile per risolvere i problemi di codifica, ma può anche aiutarci a comprendere meglio il nostro codice. Possiamo utilizzare il comando `console.log()` in più parti del nostro codice per vedere come i valori delle variabili cambiano durante l'esecuzione. Inoltre, possiamo utilizzare il metodo `console.table()` per visualizzare in modo più leggibile array e oggetti.

Un altro strumento utile per il debug è il comando `debugger`. Inserendolo all'interno del nostro codice, il processo di esecuzione si fermerà in quel punto e ci permetterà di effettuare controlli e esaminare i valori delle variabili in quel dato momento.

##Vedi Anche:

- [Console API di MDN](https://developer.mozilla.org/it/docs/Web/API/Console)
- [Debugging in JavaScript di FreeCodeCamp](https://www.freecodecamp.org/news/javascript-debugging-tutorial-how-to-debug-with-chrome-devtools-2b471a67cdb1/)
- [Guida al Debug di JavaScript di W3Schools](https://www.w3schools.com/js/js_debugging.asp)