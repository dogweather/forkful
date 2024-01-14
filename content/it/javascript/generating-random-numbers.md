---
title:    "Javascript: Generare numeri casuali"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Perché generare numeri casuali in Javascript?

Generare numeri casuali può essere utile in molti casi, come ad esempio quando si vuole creare un gioco o un programma di simulazione in cui serve un elemento di casualità. Inoltre, possono essere utili per testare algoritmi e funzioni matematiche.

## Come generare numeri casuali in Javascript

In Javascript, possiamo utilizzare la funzione `Math.random()` per generare un numero casuale compreso tra 0 e 1. Se vogliamo un range diverso, possiamo moltiplicare il risultato per il nostro numero massimo e aggiungere il numero minimo desiderato. Ad esempio, se volessimo generare un numero casuale compreso tra 1 e 10, possiamo utilizzare la formula `Math.random() * 10 + 1`.

```
```Javascript
// Genera un numero casuale tra 1 e 10
let randomNumber = Math.random() * 10 + 1;
console.log(randomNumber); // Output: 5.3452
```
```

Possiamo anche usare la funzione `Math.floor()` per arrotondare il numero casuale a un intero. Inoltre, possiamo creare una funzione che generi un numero casuale con un range personalizzato.

```
```Javascript
// Funzione che genera un numero casuale in un range specifico
function generateRandomNumber(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(generateRandomNumber(1, 100)); // Output: 57
```
```

## Approfondimento sulla generazione di numeri casuali

È importante ricordare che la funzione `Math.random()` non genera veramente numeri casuali, ma utilizza un algoritmo per generare numeri pseudo-casuali. Questo significa che i numeri generati possono non essere completamente imprevedibili. Inoltre, non è possibile controllare i numeri casuali generati da questa funzione, quindi non è adatta per scopi di sicurezza.

Per ottenere numeri pseudo-casuali più "casuali", possiamo utilizzare una libreria esterna come `crypto.getRandomValues()` che utilizza un generatore di numeri casuali basato sull'hardware del dispositivo.

# Vedi anche

- [Documentazione su `Math.random()`](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Libreria `crypto.getRandomValues()`](https://developer.mozilla.org/it/docs/Web/API/Crypto/getRandomValues)