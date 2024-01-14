---
title:                "TypeScript: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Generare numeri casuali è un'operazione comune nella programmazione, soprattutto quando si sviluppano giochi o applicazioni in cui è necessaria una dose di casualità. Anche nella creazione di test di unità è spesso utile generare input casuali per coprire diversi casi di utilizzo.

## Come fare
Per generare numeri casuali in TypeScript, possiamo utilizzare la funzione ```Math.random()```. Questa funzione restituisce un numero casuale compreso tra 0 (incluso) e 1 (escluso).

```
let randomNumber = Math.random();
console.log(randomNumber);

// Output: 0.7485123654
```

Per ottenere un numero compreso in un intervallo specifico, possiamo moltiplicare il risultato di ```Math.random()``` per la differenza tra il massimo e il minimo desiderato e sommare il minimo:

```
let min = 10;
let max = 20;
let randomNumber = Math.random() * (max - min) + min;
console.log(randomNumber);

// Output: 17.3854236797
```

Possiamo anche utilizzare la funzione ```Math.floor()``` per ottenere un numero intero anziché un numero decimale:

```
let min = 10;
let max = 20;
let randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
console.log(randomNumber);

// Output: 17
```

## Deep Dive
Nel linguaggio TypeScript, la funzione ```Math.random()``` utilizza l'algoritmo di generazione di numeri casuali di Park-Miller. Questo algoritmo è basato su un generatore lineare congruenziale (LCG), che utilizza una particolare formula matematica per generare sequenze di numeri pseudo-casuali.

Anche se la funzione ```Math.random()``` restituisce numeri pseudo-casuali, cioè numeri che sembrano casuali ma in realtà seguono una sequenza prevedibile, è possibile migliorare la casualità del risultato utilizzando altre tecniche, come l'utilizzo di timestamp o di altri parametri per inizializzare il generatore di numeri casuali.

## Vedi anche
- Documentazione ufficiale di TypeScript: https://www.typescriptlang.org/docs
- Tutorial su come generare numeri casuali in TypeScript: https://www.digitalocean.com/community/tutorials/how-to-generate-random-numbers-in-typescript
- Esempi di utilizzo di numeri casuali in giochi sviluppati in TypeScript: https://github.com/photonstorm/phaser3-examples/blob/master/src/games/random%20number.js