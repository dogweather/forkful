---
date: 2024-01-27 20:35:35.118406-07:00
description: "Generare numeri casuali in TypeScript consiste nel creare valori numerici\
  \ imprevedibili all'interno di un intervallo specificato. I programmatori\u2026"
lastmod: '2024-03-13T22:44:43.172132-06:00'
model: gpt-4-0125-preview
summary: Generare numeri casuali in TypeScript consiste nel creare valori numerici
  imprevedibili all'interno di un intervallo specificato.
title: Generazione di numeri casuali
weight: 12
---

## Cosa e perché?

Generare numeri casuali in TypeScript consiste nel creare valori numerici imprevedibili all'interno di un intervallo specificato. I programmatori sfruttano questi numeri casuali per una varietà di scopi, come generare identificatori unici, simulare dati per i test o aggiungere imprevedibilità a giochi e simulazioni.

## Come fare:

In TypeScript, è possibile generare numeri casuali utilizzando l'oggetto globale `Math`. Di seguito sono riportati alcuni esempi pratici che dimostrano come produrre numeri casuali per diverse esigenze.

### Generare un Numero Casuale di Base

Per generare un numero decimale casuale di base compreso tra 0 (incluso) e 1 (escluso), si utilizza `Math.random()`. Questo non richiede alcuna manipolazione aggiuntiva:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

Questo potrebbe produrre un valore come `0.8995452185604771`.

### Generare un Intero Casuale tra Due Valori

Quando si necessita di un intero tra due valori specifici, si incorporano sia `Math.random()` che alcune operazioni aritmetiche:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

Questo potrebbe produrre un valore intero compreso tra 1 e 10, come `7`.

### Generare un Identificatore Unico

I numeri casuali possono essere combinati con altri metodi per creare identificatori unici, per esempio, un semplice frammento di codice per generare un UUID:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

Questo genera una stringa simile a un UUID, come `110e8400-e29b-41d4-a716-446655440000`.

## Approfondimento

Il metodo principale per generare numeri casuali in JavaScript e quindi in TypeScript, `Math.random()`, si basa su un generatore di numeri pseudo-casuali (PRNG). È importante notare che, sebbene i risultati possano sembrare casuali, sono generati da un algoritmo deterministico basato su un valore di seed iniziale. Pertanto, i numeri prodotti da `Math.random()` non sono veramente casuali e non dovrebbero essere utilizzati per scopi crittografici.

Per numeri casuali crittograficamente sicuri, l'API Web Crypto offre `crypto.getRandomValues()`, che è accessibile negli ambienti che supportano lo standard Web Crypto, inclusi i browser moderni e Node.js (tramite il modulo `crypto`). Ecco un esempio rapido che illustra il suo utilizzo in TypeScript per generare un numero casuale sicuro all'interno di un intervallo:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

Questo metodo fornisce un livello di casualità più forte ed è più adatto per applicazioni sensibili alla sicurezza. Tuttavia, è anche più intensivo in termini di risorse e potrebbe non essere necessario per compiti più banali, come semplici simulazioni o generazione di valori casuali non critici.
