---
title:                "TypeScript: Testien kirjoittaminen"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testaaminen on tärkeä osa ohjelmistokehitystä. Se varmistaa, että koodi toimii oikein ja estää mahdolliset bugeja ja virheitä tulevaisuudessa. Kirjoittamalla testeja voit myös helpommin ymmärtää koodiasi ja tehdä siitä luotettavamman.

## Miten

Testien kirjoittaminen TypeScriptillä on helppoa ja tehokasta. Voit käyttää esimerkiksi Jest-kirjastoa, joka tarjoaa valmiit työkalut testien luomiseen ja ajamiseen. Tässä esimerkissä näytämme, miten voit testata yksinkertaisen funktio, joka palauttaa kahden luvun summan.

```TypeScript
const sum = (a: number, b: number) => a + b;

it('returns the sum of two numbers', () => {
  expect(sum(2, 3)).toEqual(5);
});
```

Tässä koodissa määritämme funktio sum, joka ottaa vastaan kaksi numeroa ja palauttaa niiden summan. Sen jälkeen testaamme funktiota käyttäen Jest-kirjaston `expect`- ja `toEqual`-funktioita, jotka vertaavat funktiomme palautusta odotettuun arvoon.

Voit myös testata virheitä esimerkiksi `toThrow`-funktion avulla:

```TypeScript
const divide = (a: number, b: number) => {
  if (b === 0) {
    throw new Error('Division by zero not allowed');
  } else {
    return a / b;
  }
}

it('throws an error when dividing by zero', () => {
  expect(() => divide(4, 0)).toThrow('Division by zero not allowed');
});
```

Kuten näet, testien kirjoittaminen TypeScriptillä on melko yksinkertaista. Voit myös luoda omia testejä tarpeen mukaan ja muokata niitä tarpeidesi mukaan.

## Syvemmälle

Testien kirjoittaminen ei ole vain yksinkertainen tapa varmistaa koodin toimivuus, vaan se auttaa myös parantamaan koodisi laatua. Kirjoittamalla testejä joudut ajattelemaan koodiasi tarkemmin ja huomaat mahdollisia heikkouksia ja virheitä, joita et ehkä muuten olisi huomannut.

Hyvä käytäntö on myös kirjoittaa testit ennen varsinaisen koodin kirjoittamista. Tällöin varmistat, että koodiasi on helppo testata ja se toimii odotetulla tavalla.

Voit myös integroida testit osaksi jatkuvaa integraatiota ja kehitystä (CI/CD), jotta testien ajaminen tapahtuu automaattisesti jokaisen koodimuutoksen yhteydessä. Tämä nopeuttaa koodin kehitysprosessia ja varmistaa, että muutokset eivät riko toimivaa koodia.

## Katso myös

- Jestin viralliset dokumentaatiot: https://jestjs.io/
- TypeScript-testauksen perusteet: https://www.typescriptlang.org/docs/handbook/testing.html
- Hyviä käytäntöjä testien kirjoittamisessa: https://enterprisecraftsmanship.com/posts/good-practices-for-writing-e2e-tests/