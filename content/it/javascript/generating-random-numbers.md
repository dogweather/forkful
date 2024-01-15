---
title:                "Generazione di numeri casuali"
html_title:           "Javascript: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Il generare numeri casuali è un utile strumento per diverse attività, che spaziano dalla creazione di giochi a scopo ludico fino all'elaborazione di algoritmi più complessi. Questa funzionalità è disponibile nel linguaggio Javascript attraverso la libreria Math.random ().

## Come fare
Per generare un numero casuale in Javascript, è necessario utilizzare il metodo Math.random () che restituisce un numero decimale compreso tra 0 e 1. Per convertirlo in un numero intero e allargare il range, si possono utilizzare metodi come Math.floor () e Math.ceiling (). Ad esempio, per creare un numero casuale compreso tra 1 e 10, si può utilizzare il seguente codice:
```Javascript
Math.floor(Math.random() * 10) + 1;
```
Il risultato sarà un numero intero tra 1 e 10. È possibile creare numeri casuali in diversi intervalli modificando i parametri dei metodi utilizzati.

## Approfondimento
Per comprendere meglio il funzionamento della generazione dei numeri casuali in Javascript, è importante conoscere alcuni concetti matematici alla base. Il metodo Math.random () utilizza un algoritmo chiamato "Generatore di Numeri Casuali Lineare Congruenziale" (LCG). Questo algoritmo utilizza un parametro chiamato "seed" che determina il valore iniziale del numero casuale generato. Se questo parametro viene lasciato vuoto, verrà utilizzato un seed predefinito dal browser.

## Vedi anche
- [Documentazione Math.random()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Funzionamento del Generatore di Numeri Casuali Lineare Congruenziale](https://en.wikipedia.org/wiki/Linear_congruential_generator)
- [Esempi di utilizzo di numeri casuali in Javascript](https://www.w3schools.com/js/js_random.asp)