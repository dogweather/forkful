---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:18.137242-07:00
description: "\xC5 skrive tester i TypeScript inneb\xE6rer \xE5 lage automatiserte\
  \ skript for \xE5 verifisere funksjonaliteten og riktigheten av koden din. Programmerere\
  \ gj\xF8r dette\u2026"
lastmod: '2024-03-13T22:44:40.535728-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive tester i TypeScript inneb\xE6rer \xE5 lage automatiserte skript\
  \ for \xE5 verifisere funksjonaliteten og riktigheten av koden din. Programmerere\
  \ gj\xF8r dette\u2026"
title: Skrive tester
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester i TypeScript innebærer å lage automatiserte skript for å verifisere funksjonaliteten og riktigheten av koden din. Programmerere gjør dette for å sikre pålitelighet, raskt oppdage feil og legge til rette for vedlikeholdbar kodevekst, siden TypeScript sin statiske typografi legger til et nivå av forutsigbarhet til JavaScript-testing.

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
