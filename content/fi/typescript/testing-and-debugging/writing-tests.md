---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:22.183843-07:00
description: "Testien kirjoittaminen TypeScriptill\xE4 sis\xE4lt\xE4\xE4 automatisoitujen\
  \ skriptien luomisen koodisi toimivuuden ja oikeellisuuden varmentamiseksi. Ohjelmoijat\u2026"
lastmod: '2024-03-11T00:14:30.251932-06:00'
model: gpt-4-0125-preview
summary: "Testien kirjoittaminen TypeScriptill\xE4 sis\xE4lt\xE4\xE4 automatisoitujen\
  \ skriptien luomisen koodisi toimivuuden ja oikeellisuuden varmentamiseksi. Ohjelmoijat\u2026"
title: Testien kirjoittaminen
---

{{< edit_this_page >}}

## Mikä & Miksi?
Testien kirjoittaminen TypeScriptillä sisältää automatisoitujen skriptien luomisen koodisi toimivuuden ja oikeellisuuden varmentamiseksi. Ohjelmoijat tekevät sen varmistaakseen luotettavuuden, löytääkseen virheet nopeasti ja helpottaakseen ylläpidettävän koodin kasvua, sillä TypeScriptin staattinen tyypitys lisää ennakoitavuutta JavaScript-testaukseen.

## Kuinka:
TypeScript toimii harmonisesti useimpien JavaScriptin testauskehysten kanssa. Demonstrointitarkoituksessa käytämme Jestia, suosittua testauskehystä, sen nolla-konfiguraation asetuksen vuoksi TypeScript-projekteille.

Ensiksi, varmista että sinulla on asennettuna Jest ja tarvittavat TypeScript-tyypit:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

Seuraavaksi, aseta Jest toimimaan TypeScriptin kanssa muokkaamalla `jest.config.js` tiedostoa tai luomalla uusi:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Nyt, kirjoitetaan yksinkertainen funktio ja testi sille. Harkitse `sum.ts` tiedostoa seuraavalla funktiolla:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

Luo testitiedosto nimeltä `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('lisää 1 + 2 jotta tulos on 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Suorita testisi käyttäen:

```bash
npx jest
```

Esimerkkituloste läpäisseestä testistä näyttäisi jotakuinkin tältä:

```plaintext
 PASS  ./sum.test.ts
  ✓ lisää 1 + 2 jotta tulos on 3 (2 ms)
```

Asynkronisen koodin kohdalla, Jest sopeutuu käyttämällä `async/await`. Oletetaan, että sinulla on asynkroninen `fetchData` funktio:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

Testisi käyttäen asynkronisia funktioita:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('hakee tiedot onnistuneesti', async () => {
  expect(await fetchData()).toBe('data');
});
```

Kun suoritat testejäsi, Jest odottaa lupauksen toteutumista, testaten asynkroniset operaatiot oikein.

Muista, että tehokas testaaminen sisältää useiden testien kirjoittamisen eri skenaarioille, mukaan lukien reunatapaukset, jotta varmistetaan TypeScript-koodisi toimii odotetusti.
