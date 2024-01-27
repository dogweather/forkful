---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester är som en kvalitetskontroll för koden, där man systematiskt verifierar att varje del fungerar som den ska. Programmerare gör det för att tidigt upptäcka fel, förenkla framtida underhåll och säkerställa att koden står pall över tid.

## Hur gör man:
Här är ett enkelt exempel med Jest, ett populärt testramverk för JavaScript och TypeScript. Först installerar du Jest:

```bash
npm install --save-dev jest @types/jest ts-jest
```

Ange inställningar i din `jest.config.js` för TypeScript:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Nu skriver vi ett simpelt test för en funktion som adderar två tal:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}

// sum.test.ts
import { sum } from './sum';

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Kör testet med:

```bash
npx jest
```

Exempel på utskrift:

```
PASS  ./sum.test.ts
✓ adds 1 + 2 to equal 3 (5ms)
```

## Fördjupning
Tester har alltid varit centrala i mjukvaruutveckling. På 70-talet pratade man om "debugging", men idag är "test-driven development" och "behavior-driven development" standard. Alternativ till Jest inkluderar Mocha, Jasmine och QUnit. Jest sköter båda enhetstester och integrationstester bra, och dess "snapshot" funktion är perfekt för att testa stora datamängder.

## Se även
- Jest officiell hemsida: [https://jestjs.io](https://jestjs.io)
- Artikel om testdriven utveckling (TDD): [https://en.wikipedia.org/wiki/Test-driven_development](https://en.wikipedia.org/wiki/Test-driven_development)
