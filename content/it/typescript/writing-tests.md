---
title:                "TypeScript: Scrivere test"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test è fondamentale per garantire la qualità e l'affidabilità del nostro codice. Ci permette di individuare e risolvere bug prima che il nostro software arrivi in produzione, riducendo il rischio di downtime e offrendo una migliore esperienza agli utenti.

## Come fare

Per scrivere test in TypeScript, dobbiamo prima importare la libreria `chai` che ci permette di effettuare le asserzioni e `mocha` che ci permette di organizzare i nostri test in suite.

Dopo aver installato queste dipendenze, possiamo scrivere il nostro primo test. In questo esempio, supponiamo di avere una funzione `sum` che accetta due numeri e restituisce la loro somma:

```TypeScript
const { expect } = require('chai');
const { describe, it } = require('mocha');

describe('sum function', () => {
  it ('should correctly add two numbers', () => {
    const result = sum(2,3);
    expect(result).to.equal(5);
  });
});
```

In questo esempio, abbiamo creato una suite di test chiamata "sum function" e un test all'interno di essa che verifica che la funzione `sum` restituisca il risultato corretto per due numeri dati. Utilizzando la funzione `expect` della libreria `chai`, possiamo fare asserzioni sul valore di `result`. Se la nostra asserzione non è verificata, il test fallirà e ci sarà segnalato il motivo del fallimento.

Aggiungere sempre più test che coprano diversi scenari ci garantirà una migliore copertura e affidabilità del nostro codice.

## Deep Dive

Scrivere test non solo ci aiuta a individuare e risolvere bug, ma anche a progettare un codice più modulare e testabile. Inoltre, ci offre una documentazione vivente del nostro codice che ci permette di comprenderne meglio il funzionamento nel tempo.

Per ottimizzare il nostro processo di testing, possiamo anche utilizzare altri strumenti come `istanbul` per generare una copertura dei nostri test e `eslint` per identificare eventuali errori di sintassi o best practices non seguite.

## Vedi anche

- [Guida completa alla scrittura dei test in TypeScript] (https://www.typescriptlang.org/docs/handbook/testing.html)
- [Documentazione su Mocha] (https://mochajs.org/)
- [Documentazione su Chai] (https://www.chaijs.com/)