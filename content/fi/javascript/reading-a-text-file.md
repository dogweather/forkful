---
title:    "Javascript: Tekstitiedoston lukeminen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Miksi
Tekstittiedoston lukemisella on monia käytännön sovelluksia ohjelmoinnissa. Se voi tarjota tietoa, jota tarvitaan tietokannan luomiseen, tiedon tallentamiseen tai yksinkertaisesti tiedon näyttämiseen käyttäjälle.

## Kuinka tehdä
```Javascript
const fs = require('fs');  // Tuodaan "fs" moduuli käyttöön

// Luetaan tiedosto "example.txt" ja tulostetaan sen sisältö
fs.readFile('example.txt', 'utf8', (err, data) => {  
  if (err) throw err;
  console.log(data);  
})
```

Tässä esimerkissä käytämme Node.js:ssä olevaa "fs" moduulia lukemaan tekstittiedoston käyttäen `readFile`-funktiota. Tämä funktio ottaa kolme parametria: tiedoston nimen, merkistötyypin ja virheenkäsittelyfunktion. Koodin suorituksen jälkeen tulostetaan tekstittiedoston sisältö konsoliin.

```Javascript
// Luodaan uusi tekstittiedosto "newfile.txt"
fs.writeFile('newfile.txt', 'Tämä on uusi tekstittiedosto!', 'utf8', (err) => {
  if (err) throw err;
  console.log('Tiedosto luotu!');
})
```

Tässä käytämme `writeFile`-funktiota luomaan uuden tekstittiedoston. Tämä funktio ottaa neljä parametria: tiedoston nimen, sisällön, merkistötyypin ja virheenkäsittelyfunktion. Koodin suorituksen jälkeen luodaan uusi tekstittiedosto nimellä "newfile.txt", jossa on sisältönä "Tämä on uusi tekstittiedosto!".

## Syventävä tieto
Tekstittiedoston lukeminen on vain yksi osa tiedonkäsittelyä ohjelmoinnissa. On myös tärkeää huomata, että tiedostojen lukeminen ja kirjoittaminen voi olla hidas prosessi ja voi hidastaa ohjelman suoritusta. Siksi on hyvä idea oppia käyttämään muistia ja taulukoita tietojen säilyttämiseen ja käsittelyyn sen sijaan, että jatkuvasti luodaan ja muokataan tekstittiedostoja.

## Katso myös
- [Node.js "fs" moduuli](https://nodejs.org/api/fs.html)
- [Tutorial: Reading and Writing Files in Node.js](https://stackabuse.com/reading-and-writing-files-in-node-js/)