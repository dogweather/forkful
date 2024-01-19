---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Generare numeri casuali in JavaScript riguarda la creazione di numeri che non seguono un modello prevedibile. Questo viene utilizzato dai programmatori per vari scenari, come simulazioni, test casuali, creare varietà, ecc.

## Come fare:

Creare un numero casuale tra 0 (incluso) e 1 (escluso) è semplice in JavaScript:

```Javascript
var numeroCasuale = Math.random();
console.log(numeroCasuale);
```

Se vuoi generare un numero casuale tra due numeri specifici, usa la formula seguente:

```Javascript
function generaNumeroCasuale(min, max) {
    return Math.random() * (max - min) + min;
}
console.log(generaNumeroCasuale(10, 20));
```

## Analisi Profonda:

Il metodo Math.random() è stato introdotto in JavaScript 1.0, quindi è molto datato ma affidabile. Tuttavia, il suo algoritmo di generazione di numeri casuali non è specificato e può cambiare da un browser al'altro.

Un'alternativa comune potrebbe essere l'utilizzo di una libreria, come lodash o chance, che offre funzioni per generare vari tipi di numeri casuali.

A livello di implementazione, Math.random() non restituisce un vero numero casuale. Piuttosto, restituisce un numero pseudo-casuale, in quanto viene generato da un algoritmo deterministico.

## Vedi Anche:

1. La documentazione di [Mozilla Developer Network](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Math/random) fornisce una visione più dettagliata su Math.random().
2. [Chance.js](http://chancejs.com/) e [lodash](https://lodash.com/docs/4.17.15#random) sono due eccellenti librerie per la generazione di casualità in JavaScript.
3. Un [articolo](https://medium.com/@joshuablankenship/random-number-generators-in-javascript-2ebf076ae10a) sul Medium discute i generatori di numeri casuali in JavaScript.