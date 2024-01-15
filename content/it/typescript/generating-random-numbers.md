---
title:                "Generazione di numeri casuali"
html_title:           "TypeScript: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali può essere utile in diversi contesti, come giochi, simulazioni o test di algoritmi. Con TypeScript, è possibile generare facilmente numeri casuali utilizzando alcune delle funzioni già incluse nel linguaggio.

## Come fare

Per generare numeri casuali in TypeScript, è possibile utilizzare la funzione `Math.random()`. Questa funzione restituisce un numero decimale casuale compreso tra 0 e 1, escludendo però il valore 1 stesso.

Per generare un numero intero casuale compreso tra due valori specificati, possiamo utilizzare la seguente formula:

```TypeScript
Math.floor(Math.random() * (max - min + 1)) + min;
```

Dove `max` e `min` sono rispettivamente il valore massimo e minimo del range di numeri che vogliamo generare.

Ad esempio, se volessimo generare un numero intero casuale compreso tra 1 e 10, il codice sarebbe il seguente:

```TypeScript
let randomNumber = Math.floor(Math.random() * (10 - 1 + 1)) + 1;
console.log(randomNumber); // Output: 8
```

In questo esempio, la funzione `Math.floor()` viene utilizzata per arrotondare il valore decimale restituito da `Math.random()` alla cifra intera inferiore.

Per generare un numero casuale compreso tra 0 e un valore specificato, possiamo utilizzare la formula seguente:

```TypeScript
Math.floor(Math.random() * max);
```

Dove `max` è il valore massimo (escluso) del range di numeri che vogliamo generare.

## Approfondimento

La funzione `Math.random()` utilizza un algoritmo pseudo-casuale per generare i numeri, che significa che i numeri generati non sono veramente casuali, ma seguono un preciso schema definito dall'algoritmo. Questo perché le macchine non sono in grado di generare numeri veramente casuali, ma solo numeri che sembrano casuali.

Se vogliamo ottenere numeri più casuali possiamo utilizzare librerie esterne come `crypto` o `random-js`, che utilizzano metodi di generazione basati su sorgenti di dati esterne come l'orario di sistema o l'input dell'utente.

## Vedi anche

- Documentazione ufficiale di `Math.random()`: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- Libreria `crypto`: https://www.npmjs.com/package/crypto
- Libreria `random-js`: https://www.npmjs.com/package/random-js