---
title:                "Testien kirjoittaminen"
html_title:           "TypeScript: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Testien kirjoittaminen on ohjelmistokehittäjien tärkeä tehtävä, jossa luodaan automatisoituja testejä ohjelmien toiminnan ja toimintakyvyn varmistamiseksi. Tämä helpottaa virheiden löytämistä ja korjaamista kehitysprosessin aikana ja varmistaa, että ohjelmisto toimii odotetulla tavalla.

## Kuinka: 
Esimerkki testin kirjoittamisesta käyttäen TypeScript-kieltä:
```TypeScript
function sum(a: number, b: number): number {
  return a + b;
}

test("Sum of 1 and 2 should equal 3", () => {
  expect(sum(1, 2)).toBe(3);
});
```
Output:
```
PASS Sum of 1 and 2 should equal 3
```

## Syvempää tutkiskelua:
Testien kirjoittaminen on osa ketterää ohjelmistokehitystä ja sen tavoitteena on varmistaa ohjelmiston laadukas toiminta. Historiallisesti testaaminen on ollut manuaalista ja aikaa vievää, mutta automaattisten testien avulla testien kirjoittaminen on nopeutunut ja helpottunut. TypeScript tarjoaa kattavan testauksen kirjaston, johon kuuluu mm. expect- ja toBe-funktiot, jotka helpottavat testien kirjoittamista sekä parantavat koodin luettavuutta.

## Katso myös:
Lisätietoa testien kirjoittamisesta TypeScript-kielellä löytyy esimerkiksi seuraavista lähteistä:
- https://www.typescriptlang.org/docs/handbook/testing.html
- https://jestjs.io/docs/en/expect
- https://jestjs.io/docs/en/using-matchers