---
title:                "Javascript: Testien kirjoittaminen"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen ohjelmointiprojekteissa on erittäin tärkeää, sillä ne varmistavat koodin toimivuuden ja auttavat havaitsemaan mahdolliset virheet ja bugeja ennen tuotteen julkaisua. Se myös auttaa parantamaan ohjelmointitaitoja ja edistää koodin laadun hallintaa.

## Miten

Testien kirjoittaminen voi aluksi vaikuttaa hankalalta ja aikaavievältä, mutta se maksaa itsensä takaisin pitkällä aikavälillä. Alla on esimerkkejä siitä, miten voit kirjoittaa yksinkertaisia testejä JavaScriptillä käyttäen [Jest] (https://jestjs.io/) testauskirjastoa.

````Javascript
// Testi funktiolle, joka lisää kahden luvun arvot
test('addition adds two numbers correctly', () => {
  expect(addition(1, 2)).toBe(3);
});
````

Testin suorittaminen tulisi palauttaa "passed", jos funktio toimii oikein. Voit myös lisätä virheellisiä arvoja testaukseen nähdäksesi, kuinka ohjelma käsittelee ne.

````Javascript
// Testi funktiolle, joka tarkistaa, onko syötetty arvo luku
test('check if input is a number', () => {
  expect(checkNumber(3)).toBe(true);
  expect(checkNumber('abc')).toBe(false);
});
````

## Deep Dive

Testien kirjoittamisessa on tärkeää ymmärtää, mitä haluat testata ja miksi. On myös tärkeää muistaa, että testien kirjoittaminen ei tarkoita, että koodi on täysin virheetöntä. Se auttaa kuitenkin löytämään ja korjaamaan mahdolliset virheet aikaisessa vaiheessa, mikä säästää aikaa ja vaivaa myöhemmin.

Kun kirjoitat testejä, sinun tulee myös varmistaa, että testauksen kattavuus on riittävä. Tämä tarkoittaa, että testaat kaikki mahdolliset tapaukset, jotta voit olla varma, että koodi toimii oikein kaikissa tilanteissa.

Voit myös harkita testien automatisointia ja niiden integroimista osaksi ohjelmointiprosessia. Tämä auttaa pitämään koodin laadun korkealla ja säästää aikaa manuaaliselta testaukselta.

## Katso myös

- [Jest testing library] (https://jestjs.io/)
- [The importance of writing tests in programming] (https://blog.testlodge.com/importance-writing-tests-programming/)
- [Test-driven development in JavaScript] (https://www.digitalocean.com/community/tutorials/test-driven-development-in-javascript)