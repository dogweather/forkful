---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:18.137242-07:00
description: "Hvordan: TypeScript fungerer harmonisk med de fleste JavaScript-testingrammeverk.\
  \ For demonstrasjonsform\xE5l vil vi bruke Jest, et popul\xE6rt\u2026"
lastmod: '2024-03-13T22:44:40.535728-06:00'
model: gpt-4-0125-preview
summary: TypeScript fungerer harmonisk med de fleste JavaScript-testingrammeverk.
title: Skrive tester
weight: 36
---

## Hvordan:
TypeScript fungerer harmonisk med de fleste JavaScript-testingrammeverk. For demonstrasjonsformål vil vi bruke Jest, et populært testingrammeverk, på grunn av dets nullkonfigurasjonsoppsett for TypeScript-prosjekter.

Først, sørg for at du har installert Jest og de nødvendige TypeScript-typene:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

Deretter, sett opp Jest for å fungere med TypeScript ved å modifisere `jest.config.js` eller hvis du lager en ny:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Nå, la oss skrive en enkel funksjon og en test for den. Vurder en `sum.ts`-fil med følgende funksjon:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

Opprett en testfil ved navn `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('legger til 1 + 2 for å bli 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Kjør testene dine med:

```bash
npx jest
```

Eksempelutdata som indikerer en bestått test bør se slik ut:

```plaintext
 PASS  ./sum.test.ts
  ✓ legger til 1 + 2 for å bli 3 (2 ms)
```

For asynkron kode, tilrettelegger Jest med `async/await`. Anta at du har en asynkron `fetchData`-funksjon:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

Testen din som bruker asynkrone funksjoner:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('henter data vellykket', async () => {
  expect(await fetchData()).toBe('data');
});
```

Når du kjører testene dine, vil Jest vente på at løftet skal løses, og dermed teste asynkrone operasjoner korrekt.

Husk, effektiv testing inkluderer å skrive flere tester for forskjellige scenarier, inkludert kanttilfeller, for å sikre at din TypeScript-kode oppfører seg som forventet.
