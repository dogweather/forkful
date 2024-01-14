---
title:                "Javascript: Generazione di numeri casuali"
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Perché Generare Numeri Casuali in Javascript?

Generare numeri casuali è un'operazione molto comune nella programmazione di giochi, simulazioni e applicazioni che richiedono una componente di casualità. In Javascript, esistono diverse funzioni già presenti nel linguaggio che permettono di generare numeri casuali, ma comprendere come funzionano e come utilizzarle è fondamentale per avere un controllo maggiore sui risultati ottenuti.

## Come Generare Numeri Casuali in Javascript

Per generare numeri casuali in Javascript, esistono due metodi principali: utilizzare la funzione `Math.random()` o la libreria `random-js`. Vediamo di seguito dei semplici esempi di codice per entrambi i metodi e il risultato ottenuto.

### Utilizzando la Funzione `Math.random()`

```Javascript
// Genera un numero casuale compreso tra 0 e 1
var randomNumber = Math.random();
console.log(randomNumber);
// Output: 0.5234620912308679

// Genera un numero casuale compreso tra 0 e 10
var randomInteger = Math.random() * 10;
console.log(Math.floor(randomInteger));
// Output: 7
```

Come si può vedere, utilizzando la funzione `Math.random()` possiamo generare facilmente numeri casuali compresi tra 0 e 1, ma se vogliamo ottenere un intervallo diverso possiamo moltiplicare il numero per il range desiderato e utilizzare il metodo `Math.floor()` per arrotondarlo all'intero inferiore.

### Utilizzando la Libreria `random-js`

```Javascript
// Import della libreria random-js
var Random = require('random-js');
// Creazione del seed per la generazione dei numeri casuali
var random = new Random(Random.engines.mt19937().autoSeed());

// Genera un numero casuale compreso tra 0 e 10
var randomNumber = random.integer(0, 10);
console.log(randomNumber);
// Output: 6

// Genera un numero casuale compreso tra 5 e 15
var randomInteger = random.integer(5, 15);
console.log(randomInteger);
// Output: 13
```

Utilizzando la libreria `random-js` possiamo avere maggior controllo sulla generazione dei numeri casuali. Possiamo decidere l'intervallo di numeri da cui estrarre, il tipo di distribuzione (uniforme o gaussiana) e la possibilità di utilizzare un seed per ottenere sempre gli stessi risultati.

## Approfondimento sulla Generazione di Numeri Casuali

La generazione di numeri casuali in Javascript utilizza l'algoritmo di pseudo-randomizzazione, ovvero una sequenza di numeri che sembrano casuali ma che in realtà seguono un determinato schema. Per questo motivo, la generazione di numeri casuali non è consigliata per applicazioni che richiedono un elevato grado di sicurezza, come ad esempio la crittografia.

Inoltre, è importante tenere conto del fatto che la funzione `Math.random()` restituisce sempre numeri decimali compresi tra 0 e 1, mentre la libreria `random-js` permette di ottenere numeri interi, rendendola più adatta per determinati casi d'uso.

## Vedi Anche
- [Documentazione sulla funzione `Math.random()` su MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Documentazione di `random-js`](https://github.com/ckknight/random-js)