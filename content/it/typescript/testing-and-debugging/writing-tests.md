---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:18.405889-07:00
description: "Come fare: TypeScript funziona in armonia con la maggior parte dei framework\
  \ di test JavaScript. A scopo dimostrativo, useremo Jest, un framework di test\u2026"
lastmod: '2024-03-13T22:44:43.179496-06:00'
model: gpt-4-0125-preview
summary: TypeScript funziona in armonia con la maggior parte dei framework di test
  JavaScript.
title: Scrivere test
weight: 36
---

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
