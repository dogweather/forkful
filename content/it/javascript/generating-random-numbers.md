---
title:                "Javascript: Generare numeri casuali"
simple_title:         "Generare numeri casuali"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'operazione fondamentale nella programmazione. Può essere utile per creare giochi, simulazioni o test casuali.

## Come fare

Per generare un numero casuale in Javascript, si può utilizzare il metodo `Math.random()`. Questo metodo restituisce un numero casuale compreso tra 0 (incluso) e 1 (escluso).

```
var number = Math.random();
console.log(number); // esempio di output: 0.3765410456
```

Per ottenere un numero casuale in un range specifico, si può utilizzare la formula `Math.floor(Math.random() * (max - min + 1)) + min`. Ad esempio, per generare un numero casuale compreso tra 1 e 10:

```
var randomNum = Math.floor(Math.random() * 10) + 1;
console.log(randomNum); // esempio di output: 6
```

## Approfondimento

Il metodo `Math.random()` utilizza un algoritmo pseudo-casuale per generare i numeri casuali. Ciò significa che i numeri non sono veramente casuali, ma sono generati seguendo una sequenza predefinita. Questo può essere utile per riprodurre gli stessi risultati in diversi momenti, ma non è sicuro per utilizzi in cui è richiesta una vera casualità, come nei giochi d'azzardo.

Per questo motivo, esistono anche altre tecniche per generare numeri casuali, come utilizzare il timestamp di sistema o un dispositivo hardware esterno. Inoltre, esistono anche librerie di terze parti che offrono funzionalità più sofisticate per la generazione di numeri casuali.

## Vedi anche

- [Documentazione ufficiale di Math.random()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Libreria Chance.js per la generazione di numeri casuali](https://chancejs.com/)
- [Approfondimenti sulla casualità nei calcolatori](https://en.wikipedia.org/wiki/Randomness#Randomness_in_computing)