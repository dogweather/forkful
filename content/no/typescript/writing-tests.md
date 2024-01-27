---
title:                "Skriving av tester"
date:                  2024-01-19
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrive tester betyr å lage kode som sjekker at annen kode fungerer som den skal. Programmerere tester for å forebygge feil, spare tid og forbedre kodekvaliteten.

## Hvordan gjøre det:
I TypeScript kan du skrive tester med rammeverk som Jest. Her er et eksempel:

```TypeScript
import { add } from './math';

test('adderer to tall', () => {
  expect(add(1, 2)).toBe(3);
});
```

Installer Jest med `npm install --save-dev jest @types/jest ts-jest` og kjør testen med `npm test`. Output blir:

```
PASS  ./math.test.ts
✓ adderer to tall (2 ms)
```

## Dypdykk
Testing har røtter tilbake til software-utviklingens spede begynnelse. Alternativer til Jest inkluderer Mocha, Jasmine og Tape. TypeScript krever vanligvis transpilering til JavaScript, men `ts-jest` lar oss kjøre TypeScript direkte. Testing bør inkludere enhetstester, integrasjonstester og end-til-end tester for å sikre kode i ulike miljøer.

## Se også
- [Jest Dokumentasjon](https://jestjs.io/docs/en/getting-started)
- [TypeScript Offisielt Nettsted](https://www.typescriptlang.org/)
- [Mocha.js](https://mochajs.org/)
