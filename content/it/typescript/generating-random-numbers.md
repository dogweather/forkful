---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La generazione di numeri casuali è un'espressione che si riferisce all'atto di produrre numeri in un modo che non può essere previsto logicamente. I numeri casuali sono essenziali nella programmazione perché aiutano a creare variabilità, testare la robustezza del codice e simulare eventi naturali.

## Come si fa:

Ecco un esempio semplice su come generare un numero casuale tra i valori minimo e massimo in TypeScript:

```TypeScript
function getRandom(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min)) + min;
}

console.log(getRandom(1, 100));  // Esempio di output: 38
```
In questo codice, `Math.random()` genera un numero a virgola mobile tra 0 (incluso) e 1 (escluso). Successivamente, moltiplichiamo il numero a virgola mobile per la differenza tra il massimo e il minimo e poi aggiungiamo il minimo, ottenendo così un numero intero casuale nel range desiderato.

## Approfondimenti

Storicamente, la generazione di numeri casuali era un problema complesso che ha richiesto diverse tecniche ingegnose per superarlo. Negli anni '40 e '50, queste tecniche includevano l'uso di apparecchiature meccaniche, come la ruota da roulette, per produrre sequenze di numeri apparentemente casuali.

In termini di alternative, ci sono parecchie librerie di terze parti che offrono avanzate funzionalità di generazione di numeri casuali, come crypto-js e random-js. Queste possono essere utili se avete bisogno di un livello di casualità più elevato o di distribuzioni di probabilità più complesse.

Considerando i dettagli di implementazione, `Math.random()` in JavaScript (e, di conseguenza, in TypeScript) è deterministico e non fornisce veri numeri casuali. Per conseguenza, non dovrebbe essere utilizzato per scopi critici in termini di sicurezza, come la generazione di password o chiavi di crittografia.

## Approfondisci

[Random number generation in JavaScript - Mozilla MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)

[Crypto-js library](https://github.com/brix/crypto-js)

[Random-js library](https://www.npmjs.com/package/random-js)