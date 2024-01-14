---
title:    "TypeScript: Ohjelmoinnin artikkeli: Kirjoittaminen standardivirheeseen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaessa TypeScript-ohjelmaa voi ajoittain törmätä virheisiin, jotka eivät näy tavallisessa koodin suorituksessa. Tällöin on hyödyllistä kirjoittaa virhetiedot standardierroriin, jotta ne voidaan nähdä ja käsitellä erikseen.

## Miten tehdä

Käytä `console.error()` -funktiota kirjoittaaksesi viestin standardierroriin. Esimerkiksi:

```TypeScript
let num = 10;

if (num > 5) {
  console.error("Luku on suurempi kuin 5!");
}
```

Tämä koodi tulostaa virheen standardierroriin, joka voidaan nähdä konsolissa tai selaimen kehittäjätyökaluissa:

`Luku on suurempi kuin 5!`

## Syvempi sukellus

Standardierrorin käyttö on hyödyllistä, koska se antaa mahdollisuuden eritellä ja käsitellä tiettyjä virheitä erikseen. Tällöin ohjelmaan voidaan lisätä lisälogiikkaa, joka käsittelee vain standardierroriin tulleet virheet.

Lisäksi, standardierroria käytetään usein yhdessä muiden virheenkäsittelymekanismien, kuten `try/catch` -lohkojen kanssa, joiden avulla voidaan vielä paremmin hallita ohjelman suorituksessa mahdollisesti ilmeneviä virheitä.

## Katso myös

- [Virheenkäsittely TypeScriptissä](https://www.typescriptlang.org/docs/handbook/errors.html)
- [Konsolin käyttö TypeScriptissä](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#new-for-typescript-23-log-error-warnings-on-standard-error)
- [Standardierrorin merkitys ohjelmoinnissa](https://techterms.com/definition/standard_error)