---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:25.576066-07:00
description: "Hur man g\xF6r: TypeScript fungerar harmoniskt med de flesta JavaScript-testningsramverk.\
  \ F\xF6r demonstrations\xE4ndam\xE5l kommer vi att anv\xE4nda Jest, ett popul\xE4\
  rt\u2026"
lastmod: '2024-03-13T22:44:37.659615-06:00'
model: gpt-4-0125-preview
summary: TypeScript fungerar harmoniskt med de flesta JavaScript-testningsramverk.
title: Skriva tester
weight: 36
---

## Hur man gör:
TypeScript fungerar harmoniskt med de flesta JavaScript-testningsramverk. För demonstrationsändamål kommer vi att använda Jest, ett populärt testningsramverk, på grund av dess konfigurationsfria uppsättning för TypeScript-projekt.

Först, se till att du har installerat Jest och de nödvändiga TypeScript-typerna:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

Nästa steg, sätt upp Jest för att arbeta med TypeScript genom att modifiera `jest.config.js` eller om du skapar en ny:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Nu ska vi skriva en enkel funktion och ett test för den. Betrakta en `sum.ts` fil med följande funktion:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

Skapa en testfil som heter `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('lägger till 1 + 2 för att bli 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Kör dina tester med:

```bash
npx jest
```

Exempel på utdata som indikerar ett godkänt test kan se ut ungefär så här:

```plaintext
 PASS  ./sum.test.ts
  ✓ lägger till 1 + 2 för att bli 3 (2 ms)
```

För asynkron kod hanterar Jest detta med `async/await`. Anta att du har en asynkron `fetchData` funktion:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

Ditt test som använder asynkrona funktioner:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('hämtar data framgångsrikt', async () => {
  expect(await fetchData()).toBe('data');
});
```

När du kör dina tester kommer Jest att vänta på att löftet uppfylls, och korrekt testar asynkrona operationer.

Kom ihåg, effektiv testning innefattar att skriva flera tester för olika scenarion, inklusive gränsfall, för att säkerställa att din TypeScript-kod fungerar som förväntat.
