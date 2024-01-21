---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:50:21.113695-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Generare numeri casuali è il processo di ottenere valori imprevedibili da un computer. I programmatori li utilizzano per scenari come giochi, simulazioni e sicurezza informatica.

## How to: (Come fare:)
Ecco un esempio base in TypeScript:

```typescript
function getRandomInt(max: number): number {
  return Math.floor(Math.random() * Math.floor(max));
}

console.log(getRandomInt(10)); // Potrebbe stampare un numero da 0 a 9
```

Ora, usando la libreria `crypto` per numeri più sicuri:

```typescript
const { randomInt } = require('crypto');

function getRandomSecureInt(min: number, max: number): number {
  return randomInt(min, max);
}

console.log(getRandomSecureInt(1, 10)); // Stampa un numero da 1 a 10
```

## Deep Dive (Approfondimento)
Nella programmazione, i numeri casuali erano tradizionalmente generati con funzioni come `Math.random()` in JavaScript, che è il precursore di TypeScript. Tuttavia, i valori prodotti non sono adatti per la crittografia perché prevedibili.

La libreria `crypto` di Node.js entra in gioco per i bisogni di sicurezza, utilizzando algoritmi di generazione di numeri casuali crittograficamente sicuri (CSPRNG). Sono meno prevedibili e quindi più adatti per i token di autenticazione, chiavi di crittografia, ecc.

Un'alternativa è l'utilizzo di librerie di terze parti che offrono maggiore flessibilità o algoritmi dedicati per specifici casi d'uso.

Una cosa importante da ricordare è che generare "veri" numeri casuali su un computer è difficile perché i computer seguono istruzioni definite. I metodi sopra sono pseudo-casuali, dipendenti da un seme iniziale.

## See Also (Vedi anche)
- Documentazione `Math.random()` su MDN: [MDN Math.random](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- Documentazione `crypto.randomInt()` su Node.js: [Node.js crypto.randomInt](https://nodejs.org/api/crypto.html#cryptorandomintrange-options-callback)
- Libro "JavaScript: The Good Parts" di Douglas Crockford: perfetto per approfondire JavaScript e TypeScript.