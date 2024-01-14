---
title:    "Javascript: Testien kirjoittaminen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi kirjoittaa testejä?

Testaaminen on tärkeä osa ohjelmistokehitystä, sillä se auttaa varmistamaan, että koodi toimii odotetulla tavalla. Kirjoittamalla testejä voit helposti havaita ja korjata mahdolliset virheet tai ongelmat koodissasi. Tämä säästää aikaa ja vaivaa pitkällä tähtäimellä, sillä korjatut virheet eivät aiheuta ongelmia myöhemmin ja testien avulla voit myös helposti tarkistaa, toimiiko koodi edelleen odotetusti muutosten jälkeen.

## Miten kirjoittaa testejä?

Testauksen aloittaminen voi aluksi tuntua hankalalta, mutta se on oikeasti melko yksinkertaista. Ensimmäinen asia, joka sinun tulee tehdä, on ottaa käyttöön testaustyökalu, kuten Jest. Jest on suosittu ja helppokäyttöinen testaustyökalu JavaScript-ohjelmistojen kehittämiseen. Voit asentaa Jestin käyttämällä NPM-pakettienhallintatyökalua.

```javascript
npm install --save-dev jest
```

Kun Jest on asennettu, voit aloittaa testien kirjoittamisen. Alla on esimerkki yksinkertaisesta testistä.

```javascript
const add = require('./add');

test('adds 1 + 2 to equal 3', () => {
  expect(add(1, 2)).toBe(3);
});
```

Tässä testissä luodaan funktio "add" ja testataan sen palauttama arvo kun annetaan sille parametrina luvut 1 ja 2. Jestin "expect" ja "toBe" funktioiden avulla voidaan verrata testin tulosta odotettuun arvoon. Jos testiä ajetaan "npm test" komennolla, se antaa onnistuneen tuloksen.

## Syvempi sukellus

Testien kirjoittaminen ei rajoitu vain yksinkertaisiin esimerkkeihin, vaan niitä voi kirjoittaa myös monimutkaisemmille funktioille ja luokille. Jest tarjoaa erilaisia tapoja testata esimerkiksi asynkronisia funktioita tai objekteja. Jestin sivuilta löytyy kattava dokumentaatio ja esimerkkejä erilaisista testausmenetelmistä.

On myös hyvä käytäntö kirjoittaa testejä jo koodia kirjoittaessa, jolloin vältytään suurilta virhesaumoilta koodin valmistuttua. Testien ajamista voidaan myös automatisoida, jolloin ne suoritetaan jokaisen koodimuutoksen yhteydessä. Tämä auttaa varmistamaan, että koodi pysyy toimivana ja vähentää manuaalista testaustyötä.

## Katso myös
- [Jestin kotisivut](https://jestjs.io/)
- [Jestin dokumentaatio](https://jestjs.io/docs/en/getting-started)
- [Jestin esimerkkejä](https://github.com/facebook/jest/tree/master/examples)