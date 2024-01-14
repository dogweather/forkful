---
title:    "TypeScript: Testien kirjoittaminen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-tests.md"
---

{{< edit_this_page >}}

# Miksi kirjoittaa testejä

Testien kirjoittaminen on tärkeä osa ohjelmistokehitystä, koska se auttaa varmistamaan koodin toimivuuden ja vähentämään mahdollisia virheitä ja bugeja. Se myös auttaa parantamaan koodin luettavuutta ja ylläpidettävyyttä pitkällä aikavälillä.

## Miten kirjoittaa testeJä

Testien kirjoittaminen TypeScriptillä on helppoa ja hyvä käytäntö lisätä niitä jokaiseen projektiin. Seuraavassa on muutama esimerkki, kuinka voit kirjoittaa testejä käyttäen Jest-testikirjastoa:

```TypeScript
// Esimerkki funktiosta, jota haluamme testata
function sum(a: number, b: number): number {
  return a + b;
}

// Testi funktiolle
test('Laske summa', () => {
  expect(sum(1, 2)).toBe(3); // Tarkista, että summa on oikein
  expect(sum(0, 0)).toBe(0); // Testaa nollilla
  expect(sum(-1, 2)).toBe(1); // Testaa negatiivisilla luvuilla
});
```

Tässä yksinkertaisessa esimerkissä näemme, kuinka voimme käyttää `expect` ja `toBe` -metodeja varmistaaksemme, että haluamme funktio palauttaa odotetun arvon.

## Syvällisempi perehtyminen

Testien kirjoittaminen ei ole vain yksinkertainen tapa varmistaa koodin toimivuus, vaan se auttaa myös kehittäjiä ymmärtämään paremmin omaa koodiaan ja sen toimintaa. Se myös mahdollistaa koodin refaktoroinnin helpommin ja riskittömämmin.

Testeillä on myös suuri rooli ketterässä ohjelmistokehityksessä, koska ne auttavat varmistamaan, että koodi pysyy toimivana muutosten ja uusien ominaisuuksien lisäämisen yhteydessä.

## Katso myös

- [Jest-testikirjaston kotisivu](https://jestjs.io/)
- [Yksityiskohtainen opas testien kirjoittamiseen TypeScriptillä](https://basarat.gitbooks.io/typescript/docs/testing/jest.html)
- [Blogikirjoitus: Miksi testit ovat tärkeä osa ohjelmistokehitystä](https://medium.com/javascript-scene/why-i-dont-use-asynchronous-functions-in- spite-of-availability-8052e068d45e) (eng)