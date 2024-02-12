---
title:                "Virheenjäljitystulosteiden tulostaminen"
aliases: - /fi/typescript/printing-debug-output.md
date:                  2024-01-20T17:53:39.278370-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

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
