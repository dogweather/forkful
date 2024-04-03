---
date: 2024-01-20 17:53:39.278370-07:00
description: "How to: (Kuinka tehd\xE4:) TypeScriptissa voit tulostaa debug-tietoja\
  \ k\xE4ytt\xE4en `console.log()`, `console.error()` ja muita `console`-objektin\
  \ metodeja. T\xE4ss\xE4\u2026"
lastmod: '2024-03-13T22:44:56.317964-06:00'
model: gpt-4-1106-preview
summary: "TypeScriptissa voit tulostaa debug-tietoja k\xE4ytt\xE4en `console.log()`,\
  \ `console.error()` ja muita `console`-objektin metodeja."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

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
