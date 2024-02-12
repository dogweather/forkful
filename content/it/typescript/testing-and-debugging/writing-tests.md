---
title:                "Scrivere test"
aliases:
- /it/typescript/writing-tests.md
date:                  2024-02-03T19:32:18.405889-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere test"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere test in TypeScript comporta la creazione di script automatizzati per verificare la funzionalità e la correttezza del proprio codice. I programmatori lo fanno per garantire affidabilità, individuare rapidamente i bug e facilitare la crescita del codice mantenibile, poiché la digitazione statica di TypeScript aggiunge un livello di prevedibilità ai test JavaScript.

## Come fare:
TypeScript funziona in armonia con la maggior parte dei framework di test JavaScript. A scopo dimostrativo, useremo Jest, un framework di test popolare, grazie alla sua configurazione zero per i progetti TypeScript.

Prima di tutto, assicurati di avere Jest e i tipi TypeScript necessari installati:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

Successivamente, configura Jest per lavorare con TypeScript modificando il file `jest.config.js` o creandone uno nuovo:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Ora, scriviamo una semplice funzione e un test per essa. Considera un file `sum.ts` con la seguente funzione:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

Crea un file di test chiamato `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('somma 1 + 2 uguale a 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Esegui i tuoi test con:

```bash
npx jest
```

Un esempio di output per un test superato dovrebbe apparire così:

```plaintext
 PASS  ./sum.test.ts
  ✓ somma 1 + 2 uguale a 3 (2 ms)
```

Per il codice asincrono, Jest si accomoda con `async/await`. Supponiamo di avere una funzione asincrona `fetchData`:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

Il tuo test usando funzioni asincrone:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('recupera dati con successo', async () => {
  expect(await fetchData()).toBe('data');
});
```

Quando esegui i tuoi test, Jest attende che la promise sia risolta, testando correttamente le operazioni asincrone.

Ricorda, un test effettivo include la scrittura di più test per diversi scenari, inclusi i casi limite, per garantire che il tuo codice TypeScript si comporti come previsto.
