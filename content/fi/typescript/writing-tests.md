---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testaus tarkoittaa koodin toiminnallisuuden varmistamista automatisoiduilla tarkistuksilla. Ohjelmoijat tekevät sitä, koska se auttaa bugien löytämistä, parantaa koodin laatua ja tuo mielenrauhaa.

## How to:
Testataan `add`-funktiota, joka summaa numeroita. Käytetään TypeScriptissä suosittua testauskirjastoa, Jest.

```typescript
// sum.ts
export function add(a: number, b: number): number {
  return a + b;
}
```

Tehdään testi `add`-funktiolle:

```typescript
// sum.test.ts
import { add } from './sum';

test('summaa kaksi numeroa', () => {
  expect(add(1, 2)).toBe(3);
});
```

Suoritetaan testit:

```shell
$ jest
PASS  ./sum.test.ts
✓ summaa kaksi numeroa (3ms)
```

## Deep Dive
Testien kirjoittaminen on kehittynyt massiivisesti vuosien saatossa. Aluksi testaus oli yksinkertaista assertti-koodia, kunnes kehitettiin erikoistuneempia työkaluja, kuten JUnit Javaan ja sen sisarprojekti Jest JavaScript-ympäristöön. Vaihtoehtoja on monia: Mocha, Jasmine, ja QUnit ovat vain joitakin suosittuja. TypeScriptissä kannattaa huomioida tyypitetyt testikehykset ja mahdolliset integraatio-ongelmat.

## See Also
- Jest dokumentaatio: [https://jestjs.io/](https://jestjs.io/)
- TypeScript: [https://www.typescriptlang.org/](https://www.typescriptlang.org/)
- Mocha: [https://mochajs.org/](https://mochajs.org/)
- Jasmine: [https://jasmine.github.io/](https://jasmine.github.io/)
