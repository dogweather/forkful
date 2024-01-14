---
title:                "Javascript: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikainen tiedosto?

Väliaikaiset tiedostot ovat tärkeitä ohjelmoinnissa monista syistä, kuten väliaikaisten tietojen tallentamiseen, väliaikaisen varmuuskopion luomiseen ja väliaikaisen tallennustilan tarjoamiseen ohjelman suorituksen aikana. Ne ovat myös hyödyllisiä testaamisessa ja debuggaamisessa, sillä niitä voidaan käyttää tallentamaan väliaikaisesti erilaisia muuttujia ja tulosteita.

## Kuinka luoda väliaikainen tiedosto?

Jotta voit luoda väliaikaisen tiedoston Javascriptillä, sinun tarvitsee ensin importoida tarvittavat moduulit. Tämän jälkeen voit käyttää `fs.mkdtemp()` -funktiota luodaksesi väliaikaisen hakemiston ja `path.join()` -funktiota yhdistääksesi tiedoston nimi ja polun. Lopuksi voit luoda tiedoston `fs.writeFile()` -funktiolla. Katso alla oleva esimerkki:

```Javascript
const fs = require('fs');
const path = require('path');

const tempDir = fs.mkdtempSync('tmp'); // luo väliaikaisen hakemiston nimeltä "tmp"
const tempFilePath = path.join(tempDir, 'temp.txt'); // yhdistää polun ja tiedoston nimen
fs.writeFile(tempFilePath, 'Tämä on väliaikainen tiedosto', (err) => {
    if (err) throw err;
    console.log('Väliaikainen tiedosto on luotu!');
});
```

Tämän koodin suorittamisen jälkeen sinun tulisi löytää uusi väliaikainen tiedosto nimeltä "temp.txt" väliaikaisesta hakemistosta.

## Syvempi sukellus väliaikaisten tiedostojen luomiseen

Väliaikaiset tiedostot ovat usein tarpeen ohjelmoinnissa, mutta ne voivat myös aiheuttaa turhia turvallisuusriskejä. Tällöin on tärkeää varmistaa, että väliaikaiset tiedostot poistetaan asianmukaisesti ja turvallisesti niiden käytön jälkeen. Voit käyttää `fs.unlink()` -funktiota poistamaan väliaikaisen tiedoston, kun se ei ole enää tarpeen. On myös hyvä käyttää `crypto.randomBytes()` -funktiota luomiseen satunnainen merkkijono, jota voidaan käyttää tiedoston nimessä lisäämään turvallisuutta.

## Katso myös

- [Node.js:n virallinen dokumentaatio - fs-moduuli](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [W3Schools - Javascript Kirjoittamisfunktiot](https://www.w3schools.com/nodejs/ref_fs_removefile.asp)