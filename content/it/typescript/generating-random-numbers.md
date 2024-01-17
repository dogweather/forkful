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

## Cos'è e perché?
Generare numeri casuali è un processo utilizzato dai programmatori per ottenere valori casuali durante l'esecuzione di un programma. Questi numeri possono essere utilizzati per una varietà di scopi, come ad esempio simulazioni, giochi e generazione di password.

## Come fare:
Per generare numeri casuali in TypeScript possiamo utilizzare la funzione `Math.random()` che restituisce un numero compreso tra 0 e 1 (escluso). Possiamo moltiplicare questo valore per il range di numeri desiderato e aggiungere un offset per ottenere numeri interi o decimali. Ecco un esempio che genera un numero intero casuale compreso tra 1 e 10:
```TypeScript
const randomInt = Math.floor(Math.random() * 10) + 1;
console.log(randomInt); // output: 5
```

Possiamo anche creare una funzione che accetta due parametri `min` e `max` e restituisce un numero casuale compreso tra di essi:
```TypeScript
function getRandomNum(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}
const randomNum = getRandomNum(5, 10);
console.log(randomNum); // output: 8
```

## Approfondimento:
La generazione di numeri casuali è stata storicamente un problema complesso per i programmatori, poiché i computer seguono algoritmi deterministici e non possono generare veri numeri casuali. Pertanto, i metodi per generare numeri pseudo-casuali sono stati sviluppati e sono ancora ampiamente utilizzati al giorno d'oggi. Tuttavia, con l'avvento della tecnologia blockchain e della crittografia, sono state sviluppate nuove metodologie per generare numeri casuali affidabili, come ad esempio l'utilizzo di hash dei blocchi per ottenere numeri imprevedibili.

Esistono anche altre alternative alla funzione `Math.random()`, come ad esempio l'utilizzo di librerie di terze parti come `random-js` o di API esterne che forniscono numeri casuali basati su fattori esterni, come la temperatura o il movimento del mouse.

Nella programmazione, è importante utilizzare un generatore di numeri casuali che sia robusto e sicuro per evitare di compromettere la sicurezza dei dati sensibili.

## Vedi anche:
- Documentazione della funzione `Math.random()` in TypeScript: https://www.typescriptlang.org/docs/handbook/global-objects.html#math
- Libreria di terze parti `random-js`: https://github.com/ckknight/random-js
- Generazione di numeri casuali sicuri in ambienti blockchain: https://medium.com/blockchain101/true-randomness-in-blockchains-5fa31c65367f