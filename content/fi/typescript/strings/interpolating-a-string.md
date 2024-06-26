---
date: 2024-01-20 17:51:42.701699-07:00
description: "How to: (Kuinka tehd\xE4:) Toinen esimerkki, jossa k\xE4ytet\xE4\xE4\
  n laskentaa."
lastmod: '2024-04-05T21:53:57.859451-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Toinen esimerkki, jossa k\xE4ytet\xE4\xE4n laskentaa."
title: Merkkijonon interpolointi
weight: 8
---

## How to: (Kuinka tehdä:)
```typescript
// Alustetaan muuttuja
let username: string = "Tuomas";

// Interpoloidaan string literalin sisällä
let greeting: string = `Hei, ${username}! Tervetuloa!`;

console.log(greeting); // Tulostaa: Hei, Tuomas! Tervetuloa!
```

Toinen esimerkki, jossa käytetään laskentaa:

```typescript
let itemPrice: number = 19.95;
let taxRate: number = 0.24;
let finalPrice: string = `Tuotteen loppuhinta on: ${itemPrice * (1 + taxRate)} euroa.`;

console.log(finalPrice); // Tulostaa: Tuotteen loppuhinta on: 24.734 euroa.
```

## Deep Dive (Syväsukellus)
Ennen kuin ECMAScript 2015 (ES6) toi template literals -ominaisuuden JavaScriptiin, kehittäjät käyttivät `+` operaattoria tai vanhempia menetelmiä stringien yhdistämiseen. Interpolointi vie kuitenkin vähemmän tilaa ja on helpommin ymmärrettävää.

Vaihtoehtoina interpoloinnille on yhä vanhan koulukunnan yhdistely plussa-operaattorilla tai `String.concat`-metodilla, tosin nämä tekevät koodista helposti sekavamman.

Implementaatiotasolla TypeScript kääntää interpoloidut stringit tavalliseen JavaScriptiin käyttäen template literals -syntaksia tai vanhempaa syntaksia, riippuen kohde-ympäristön tukemista ominaisuuksista.

## See Also (Katso myös)
- [Template literals (Template strings) MDN Web Docsissa](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [TypeScript Handbook: Template Strings](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
