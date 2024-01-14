---
title:                "Javascript: Textitiedoston kirjoittaminen"
simple_title:         "Textitiedoston kirjoittaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on olennainen osa ohjelmointia kaikilla ohjelmointikielillä, myös Javascriptillä. Tekstitiedoston kirjoittaminen voi olla hyödyllistä, kun haluat tallentaa tietoja tai tekstejä, jotka eivät näy suoraan käyttäjälle, kuten virheenkorjausviestejä tai tietokannan sisältöä. Se on myös tärkeää esimerkiksi tekstimuotoisten tiedostojen lataamisessa tai luomisessa.

## Kuinka tehdä

Käyttämällä Javascriptiä voit helposti kirjoittaa tekstiä tiedostoon. Voit käyttää Node.js-kirjastoa tai HTML5 File API:a selaimessa. Alla olevassa esimerkissä käytämme Node.js:ää.

```Javascript
const fs = require('fs');

// Luodaan muuttuja, joka sisältää tiedostoon kirjoitettavan tekstin
let text = 'Tämä on tekstiä, joka kirjoitetaan tiedostoon.';

// Käytetään fs-kirjaston writeFile-metodia kirjoittamaan teksti tiedostoon
// Ensimmäinen parametri on tiedoston nimi, toinen on kirjoitettava teksti
fs.writeFile('tekstitiedosto.txt', text, (error) => {
  if (error) throw error;
  console.log('Teksti kirjoitettu tiedostoon!');
});

```

Yllä olevassa esimerkissä käytetään writeFile-metodia, joka ottaa ensimmäisenä parametrinä tiedoston nimen, johon teksti kirjoitetaan, ja toisena parametrina tekstiä, joka kirjoitetaan. On myös mahdollista asettaa kolmas parametri, joka on funktio, joka suoritetaan, kun kirjoitus on valmis. Tässä esimerkissä me tulostamme konsoliin viestin, joka ilmoittaa kun teksti on kirjoitettu onnistuneesti.

## Syventävä tieto

Kun kirjoitat tekstiä tiedostoon Javascriptillä, on tärkeää ymmärtää, mikä tiedostomuoto sopii tekstin tallentamiseen. Yleensä CSV- tai JSON-tiedostot toimivat hyvin, mutta voit myös tallentaa tekstin vaikkapa HTML-tiedostona, jos haluat luoda verkkosivun sisältöä automaattisesti.

Voit myös käyttää Node.js-kirjastoa lukemaan tiedostosta, jonka olet juuri kirjoittanut, jotta voit varmistaa että teksti kirjoitettiin oikein. Tämä on erityisen tärkeää, jos kirjoitat tietoa, jota käytetään myöhemmin esimerkiksi tietokannoissa tai verkkosivuilla.

## Katso myös

- [Node.js file system -kirjasto](https://nodejs.org/api/fs.html)
- [HTML5 File API](https://developer.mozilla.org/en-US/docs/Web/API/File_API)
- [CSV-tiedostojen luominen Javascriptillä](https://www.npmjs.com/package/json2csv)
- [JSON-tiedostojen lukeminen ja kirjoittaminen Javascriptillä](https://stackoverflow.com/questions/19706046/how-to-read-an-external-local-json-file-in-javascript/19706620)