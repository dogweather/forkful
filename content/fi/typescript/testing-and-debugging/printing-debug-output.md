---
date: 2024-01-20 17:53:39.278370-07:00
description: "Kehitt\xE4ess\xE4 p\xE4\xE4see joskus p\xE4lk\xE4h\xE4st\xE4 tulostamalla\
  \ debug-tietoa, joka valottaa ohjelman tilaa reaaliajassa. Se auttaa bongaamaan\
  \ virheet ja ymm\xE4rt\xE4m\xE4\xE4n\u2026"
lastmod: '2024-03-13T22:44:56.317964-06:00'
model: gpt-4-1106-preview
summary: "Kehitt\xE4ess\xE4 p\xE4\xE4see joskus p\xE4lk\xE4h\xE4st\xE4 tulostamalla\
  \ debug-tietoa, joka valottaa ohjelman tilaa reaaliajassa. Se auttaa bongaamaan\
  \ virheet ja ymm\xE4rt\xE4m\xE4\xE4n\u2026"
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

## What & Why? (Mitä & Miksi?)
Kehittäessä pääsee joskus pälkähästä tulostamalla debug-tietoa, joka valottaa ohjelman tilaa reaaliajassa. Se auttaa bongaamaan virheet ja ymmärtämään koodin käytöstä.

## How to: (Kuinka tehdä:)
TypeScriptissa voit tulostaa debug-tietoja käyttäen `console.log()`, `console.error()` ja muita `console`-objektin metodeja. Tässä pari esimerkkiä:

```TypeScript
function calculateSum(a: number, b: number): number {
  console.log('calculateSum called with:', a, b);
  return a + b;
}

const result = calculateSum(3, 4);
console.log('The result is:', result);
```

Tämä tuottaa:
```
calculateSum called with: 3 4
The result is: 7
```

Käytä `console.error()` virhetilanteisiin:
```TypeScript
function handleError(error: Error): void {
  console.error('An error occurred:', error.message);
}

try {
  // Something that might throw an error
} catch (error) {
  handleError(error);
}
```

## Deep Dive (Sukellus syvemmälle)
Debug-tulostukset ovat vanha ja luotettava työkalu. Alkuaan tulostimille suunniteltu, nykyään käytetään lähinnä kehitystyökalujen konsolissa. Vaihtoehtoina ovat monimutkaisemmat debuggaustyökalut ja IDE-integroidut ratkaisut, jotka tarjoavat täyden hallinnan ja seurannan. Käytäntö yksinkertaa: debug-tulosteiden tulisi olla informatiivisia, mutta niiden ei tulisi pullollaan tuotantokoodia. Hiljaa hyvä tulee.

## See Also (Katso myös)
Lisätietoa ja resurssit TypeScriptin ja debug-tulostuksen syvimpään ymmärrykseen:

- [TypeScript Handbook - Console](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#the-any-type): Opas TypeScriptin perustyypeistä ja `console`-objektin käytöstä.
- [MDN Web Docs - Console](https://developer.mozilla.org/en-US/docs/Web/API/Console): Tietoa `console`-objektista ja sen metodeista.
- [Visual Studio Code - Debugging](https://code.visualstudio.com/docs/editor/debugging): Ohjeet Visual Studio Code -editorin debug-ominaisuuksien käyttöön.
