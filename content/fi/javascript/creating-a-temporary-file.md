---
title:                "Tilapäistiedoston luominen"
html_title:           "Javascript: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Monissa tilanteissa, ohjelmoijan täytyy luoda tilapäisiä tiedostoja käsiteltäviksi tai tallennettaviksi väliaikaisesti. Tämä voi olla tarpeellista esimerkiksi ohjelman suorituskyvyn parantamiseksi tai työnkulun helpottamiseksi.

## Miten

Tilapäisten tiedostojen luominen Javascriptillä on helppoa. Käytämme tätä esimerkkinä fs-moduulia, joka on osa Node.js:ää. Ensimmäiseksi meidän täytyy tuoda tämä moduuli käyttöömme:

```Javascript
const fs = require('fs');
```

Sitten voimme luoda tilapäisen tiedoston käyttämällä `fs.mkdtemp()` -funktiota ja antamalla sille hakemistoja nimen. Tämä funktio palauttaa tiedoston nimen:

```Javascript
const tempDir = fs.mkdtempSync('temp-');
console.log(tempDir);
// Output: temp-QJGcNp
```

Ja siinä se! Nyt meillä on oikea hakemisto, ja voimme käsitellä sitä tarpeen mukaan. Muista kuitenkin poistaa tämä tiedosto, kun sitä ei enää tarvita, jotta voit pitää järjestelmän puhtaana. Voit tehdä sen käyttämällä `fs.rmdirSync()` -funktiota:

```Javascript
fs.rmdirSync(tempDir);
```

## Syventyvä tarkastelu

`fs.mkdtemp()` -funktiossa on myös optio lisätä hakemiston sijainti parametrina. Tämä voi olla hyödyllistä, jos haluat tallentaa tilapäiset tiedostot tiettyyn paikkaan. Voit tehdä sen antamalla toisen parametrin funktiolle. Esimerkiksi:

```Javascript
const tempDir = fs.mkdtempSync('temp-', { dir: __dirname });
console.log(tempDir);
// Output: /Users/inariyama/temp-3ys5da
```

Tulostuksessa voit nähdä, että hakemisto sijaitsee tiedoston suoritushakemistossa. Voit myös antaa kolmannen parametrin, joka määrittää, käytätäänkö tämän hakemiston luomista. Oletuksena tämä on UTF-8, mutta voit vaihtaa sen haluamasi koodauksen mukaan. Tämä voi olla hyödyllistä, jos haluat varmistaa, että tiedostot tallennetaan oikeilla kirjoituskoneilla.

See Also

- [Node.js Documentation: fs module](https://nodejs.org/api/fs.html)
- [W3Schools: Node.js File System](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)