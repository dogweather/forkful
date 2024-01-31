---
title:                "Scrivere test"
date:                  2024-01-19
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"

category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere test significa creare codice specifico per verificare altri pezzi di codice. I programmatori lo fanno per assicurarsi che funzioni tutto come previsto e per prevenire bug.

## Come si fa:
```TypeScript
import { expect } from 'chai';
import { somma } from './somma';

describe('Test della funzione somma', () => {
  it('dovrebbe ritornare 4 quando somma 2 + 2', () => {
    expect(somma(2, 2)).to.equal(4);
  });

  it('dovrebbe ritornare 0 quando somma -2 + 2', () => {
    expect(somma(-2, 2)).to.equal(0);
  });
});

// somma.ts
export function somma(a: number, b: number): number {
  return a + b;
}
```
Risultati dei test:
```
  Test della funzione somma
    ✓ dovrebbe ritornare 4 quando somma 2 + 2
    ✓ dovrebbe ritornare 0 quando somma -2 + 2
```

## Approfondimento
La scrittura di test ha radici nella pratica dello sviluppo guidato da test (TDD) degli anni '90. Alternative includono test manuali o "Expo facto testing", ma i test automatici come unit test, integration test e E2E test sono lo standard. Per TypeScript, librerie come Jest, Mocha/Chai, e Jasmine semplificano la scrittura di test, gestendo l'isolamento, la simulazione e l'affermazione dei risultati.

## Vedi Anche
- [Jest](https://jestjs.io/) - Una libreria di test JavaScript con un focus sulla semplicità.
- [Mocha](https://mochajs.org/) - Un framework per test JavaScript che funziona sia su Node.js sia nei browser.
- [Chai](https://www.chaijs.com/) - Una libreria di asserzione BDD/TDD che può essere accoppiata con qualsiasi framework di test JavaScript.
