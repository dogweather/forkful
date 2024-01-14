---
title:                "Javascript: Tarkistetaan, onko hakemisto olemassa."
simple_title:         "Tarkistetaan, onko hakemisto olemassa."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi
Javascript-ohjelmoijat usein kohtaavat tarpeen tarkistaa, onko tiettyä hakemistoa olemassa. Tämä voi johtua esimerkiksi tarpeesta luoda uusia hakemistoja tai tarkistaa, onko tietty hakemisto jo olemassa ennen kuin sinne tallennetaan tiedostoja. Tässä blogikirjoituksessa käsitellään, miten voit tarkistaa, onko hakemisto olemassa Javascriptissä.

## Kuinka
Voit tarkistaa, onko hakemisto olemassa käyttämällä Node.js:ää. Ensinnäkin, sinun täytyy tuoda fs-moduuli, joka on sisäänrakennettu Node.jseem. Sitten, voit käyttää fs.existsSync()-funktiota, joka hyväksyy polun parametrina ja palauttaa boolean-arvon riippuen siitä, onko tiedostoa olemassa vai ei.

```Javascript
const fs = require('fs');
const directory = "./hakemisto";

if(fs.existsSync(directory)) {
  console.log(`Hakemisto ${directory} löytyy.`);
} else {
  console.log(`Hakemistoa ${directory} ei löydy.`);
}
```
```Javascript
// Jos hakemisto löytyy, tulostuu:
// Hakemisto ./hakemisto löytyy.

// Jos hakemistoa ei löydy, tulostuu:
// Hakemistoa ./hakemisto ei löydy.
```

## Syvempää tietoa
Voit myös tarkistaa, onko hakemisto olemassa käyttämällä fs.statSync()-funktiota, joka palauttaa tiedoston tiedot, jos kyseinen tiedosto on olemassa. Jos tiedostoa ei ole, funktio palauttaa virheen. Voit käyttää tätä tietoa tarkistamaan, onko kyseessä hakemisto vai ei. Jos funktio palauttaa virheen, voit olettaa, että kyseessä on hakemisto.

```Javascript
const fs = require('fs');
const directory = "./hakemisto";

try {
  const stats = fs.statSync(directory);
  if (stats.isDirectory()) {
    console.log(`${directory} on hakemisto.`);
  } else {
    console.log(`${directory} ei ole hakemisto.`);
  }
} catch (err) {
  console.log(`Virhe: ${err}`);
}
```

## Katso myös
- [Node.js fs-moduuli - virallinen dokumentaatio](https://nodejs.org/docs/latest-v12.x/api/fs.html)
- [W3Schools - Javascript-directoryn luominen ja poistaminen](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Node.js - hakemistojen käsittely](https://www.geeksforgeeks.org/how-to-create-a-directory-using-node-js/)