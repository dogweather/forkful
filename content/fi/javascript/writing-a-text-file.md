---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Javascript: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi kirjoittaa tekstitiedostoa? On monia syitä, mutta muutamia yleisimpiä ovat tallennustilan säästäminen, tiedon jakaminen ja koodin dokumentointi.

## Miten

Käyttämällä Javascriptia, voit helposti luoda ja kirjoittaa tekstitiedoston sisältöä. Tässä on yksinkertainen esimerkki:

```Javascript
let fs = require('fs'); // Avaaminen ja kirjoittaminen tiedostoon "testi.txt"
fs.writeFile('testi.txt', 'Tervetuloa lukijoille!', function (err) {
  if (err) throw err;
  console.log('Tiedot tallennettu!');
});
```

Tämän koodin suorittamisen jälkeen löydät "testi.txt" tiedostosta "Tervetuloa lukijoille!" sisällön.

Voit myös lisätä sisältöä jo olemassa olevaan tekstitiedostoon käyttämällä "appendFile" -toimintoa, kuten tässä:

```Javascript
fs.appendFile('testi.txt', '\n Tämä on uusi rivi!', function (err) {
  if (err) throw err;
  console.log('Tiedot lisätty!');
});
```

Nyt "testi.txt" -tiedostossa on kaksi riviä: "Tervetuloa lukijoille!" ja "Tämä on uusi rivi!".

## Syvällinen sukellus

Kirjoittaessa tekstitiedostoa Javascriptilla, voit valita erilaisia datatyyppejä, kuten merkkijonoja, numeroita ja taulukoita. Käytä "toString ()" -toimintoa muuttaaksesi erilaiset datatyypit merkkijonoiksi ja lisätä ne tiedostoon.

Voit myös käyttää "readFile" -toimintoa lukeaksesi tiedostosta ja tallentaa sisällön muuttujaan. Tämä voi olla hyödyllistä, jos haluat käyttää tiedoston sisältöä myöhemmin koodissasi.

## Katso myös

Täältä löydät lisätietoa Javascriptin käytöstä tekstitiedostojen kanssa:

- https://www.w3schools.com/nodejs/nodejs_filesystem.asp
- https://nodejs.org/api/fs.html
- https://www.geeksforgeeks.org/javascript-program-to-write-data-in-a-text-file/