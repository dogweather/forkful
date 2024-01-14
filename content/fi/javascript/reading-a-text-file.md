---
title:    "Javascript: Tiedoston lukeminen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstilukemisen taito on tärkeä monille ohjelmoijille, erityisesti jos he työskentelevät tiedostojen kanssa. Natiivi JavaScript-kielen avulla voit helposti lukea ja käsitellä tekstiä sisältäviä tiedostoja.

## Kuinka

Tekstilukemisen avaaminen JavaScriptillä on yksinkertaista. Voit käyttää esimerkiksi readFile-metodia, joka on saatavilla Node.js: ssä. Tämä metodi avaa ja lukee tiedoston sisällön ja palauttaa sen merkkijonona.

```Javascript
const fs = require('fs');
const data = fs.readFile('tekstitiedosto.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

Tämä antaa meille tiedoston sisällön, joka voidaan tallentaa muuttujaan ja käsitellä halutulla tavalla. Voit myös käyttää HTML5 FileReader API -sovellusta, jos haluat tehdä tämän selaimessa.

## Syväsukellus

Kun haluat lukea tekstiä sisältäviä tiedostoja, on tärkeää ottaa huomioon tiedostomuoto ja mahdolliset virheet, jotka voivat ilmetä. Esimerkiksi jos tiedostosi on iso, saatat kohdata suorituskykyongelmia. Voit myös ajaa erilaisia operaatioita tiedoston kanssa, kuten tiedostojen yhdistämisen tai liittämisen.

On myös tärkeää ottaa huomioon tiedoston koodaus, sillä eri kielet käyttävät erilaisia koodaustyyppejä. Sinun on varmistettava, että tiedoston sisältämä teksti muunnetaan oikein sen koodauksesta riippuen.

## Katso myös

- [Node.js - tiedostojen lukeminen](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- [HTML5 FileReader API -sovellus](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- [Tiedoston koodauksen huomioiminen JavaScriptillä](https://www.w3docs.com/snippets/javascript/how-to-detect-the-text-encoding-of-a-file.html)