---
title:    "Javascript: Testien kirjoittaminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Miksi testin kirjoittaminen on tärkeää ohjelmointiprosessissa? Vaikka se voi tuntua ylimääräiseltä työltä, testien kirjoittaminen auttaa varmistamaan koodisi toimivuuden ja vähentää virheiden määrää. Testit myös auttavat havaitsemaan mahdollisia ongelmia jo ennen kuin koodi julkaistaan.

## Miten

Testien kirjoittaminen on helppoa JavaScriptillä käyttäen kirjastoa kuten Jest. Aloita määrittelemällä testifunktio ja antamalla sille kuvaava nimi. Kirjoita sitten testejä sisältävät koodirivit ja odotettu tulos, käyttäen Jestin funktioita kuten `expect()` ja `toBe()`. Lopuksi lisää testitiedosto projektiisi ja aja testit komentoriviltä. Voit myös asettaa testien ajamisen automaattisesti jokaisen koodimuutoksen yhteydessä, mikä auttaa varmistamaan, että aina uuden koodin lisäämisen jälkeen kaikki edelliset testit läpäisevät.

```Javascript
test("Testifunktio", () => {
  expect(funktio(1)).toBe("tulos");
});
```

## Syvempi sukellus

Testien kirjoittamisen lisäksi on tärkeää miettiä, mitä testejä kannattaa kirjoittaa ja mihin kohdistaa testaus. Kirjoita testejä koodin olennaisiin osiin ja keskity perustoimintojen testaamiseen. Vältä kirjoittamasta liian monimutkaisia testejä, sillä silloin niiden ylläpito voi olla hankalaa.

#### Tekninen velka
Tekninen velka tarkoittaa lyhyen tähtäimen ongelmien välttämistä pidemmän tähtäimen kustannuksella. Testien kirjoittamatta jättäminen voi johtaa teknisen velan kertymiseen, joka voi aiheuttaa suuria vaikeuksia myöhemmin. Pidä siis huolta, että pysyt teknisesti velkavapaana kirjoittamalla testit heti alusta lähtien.

## Katso myös

- [Jest-dokumentaatio](https://jestjs.io/)
- [JavaScript-testauksen perusteet](https://www.pluralsight.com/courses/javascript-testing-collections)
- [Testauksen tärkeys ohjelmistokehityksessä](https://blog.testlodge.com/why-test-software/)