---
title:                "Testien kirjoittaminen"
html_title:           "Javascript: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Yksi tärkeimmistä asioista ohjelmoinnissa on varmistaa, että koodimme toimii oikein ja pysyy toimivana jatkossakin. Kirjoittamalla testejä voimme vahvistaa, että koodimme toimii halutulla tavalla ja löytää mahdolliset virheet ennen kuin ne aiheuttavat ongelmia.

## Miten se tehdään

Kirjoittamalla testejä voimme tarkistaa, että koodimme tuottaa halutunlaisia tuloksia ja toimii odotetulla tavalla. Voimme käyttää esimerkiksi Jest-kirjastoa, joka on suosittu testauskirjasto JavaScript-sovelluksille.

```Javascript
// Tehdään funktio, joka laskee kahden luvun summan
function sum(x, y) {
  return x + y;
}

// Testataan, että funktio toimii halutulla tavalla
test('sum-funktio laskee summan oikein', () => {
  expect(sum(2, 2)).toBe(4);
});
```

Testien avulla voimme myös varmistaa, että koodimme ei aiheuta odottamattomia sivuvaikutuksia, joita voisi olla vaikea havaita muuten.

```Javascript
// Tehdään funktio, joka lisää uuden arvon taulukkoon
function addItem(array, item) {
  array.push(item);
}

// Testataan, että funktio lisää uuden arvon taulukkoon oikein
test('addItem-funktio lisää uuden arvon taulukkoon', () => {
  const array = [1, 2, 3];
  addItem(array, 4);
  expect(array).toHaveLength(4);
  expect(array).toContain(4);
});
```

## Syvempi sukellus

Testien kirjoittaminen auttaa myös parantamaan koodimme laatua ja ylläpidettävyyttä. Kun lisäämme uusia ominaisuuksia tai teemme muutoksia koodiin, voimme suorittaa testeistä ja varmistaa, että kaikki toimii odotetulla tavalla. Tämä auttaa myös tunnistamaan mahdollisia ristiriitaisuuksia tai ongelmia eri osien välillä.

Yksi tärkeä seikka testeissä on myös niiden avulla dokumentoida koodiamme ja sen toimintaa. Kirjoittamalla selkeitä ja ymmärrettäviä testejä, voimme myös auttaa muita kehittäjiä ymmärtämään koodiamme ja sen tarkoitusta.

## Katso myös

- [Jest-kirjaston virallinen sivusto](https://jestjs.io/)
- [Testien kirjoittamisen perusteet](https://medium.com/@zoeames/testing-javascript-what-is-a-test-8cf2a9df1a50)
- [Hyviä käytäntöjä testien kirjoittamisessa](https://medium.freecodecamp.org/the-right-way-to-test-react-components-548a4736ab22)