---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Tekstitiedoston lukeminen on prosessi, jossa tietokoneohjelma tulkitsee ja hakee tietoa tiedostosta, joka koostuu tekstistä. Ohjelmoijat tarvitsevat tätä toimintoa usein, koska se on tehokas tapa käyttää, tallentaa ja jakaa tietoja ohjelmissa.

## Näin se toimii:
Tässä on yksinkertaisen tekstitiedoston lukemiskoodin esimerkki TypeScriptillä käyttäen Node.js:n sisäänrakennettua `fs`-moduulia:

```TypeScript
import fs from 'fs';

fs.readFile('/polku/tiedostoon.txt', 'utf-8' , (virhe, data) => {
  if (virhe) {
    console.error("Tiedostoa ei voitu lukea: ", virhe);
    return;
  }
  console.log(data);
});
```

Koodin suorittaminen lukee tiedoston sisällön ja tulostaa sen konsoliin. Jos tiedostoa ei voitu lukea, konsoliin tulostuu virheilmoitus.

## Syvempi sukellus
1. Historiallinen tausta: Tiedostoja on luettu tietokoneohjelmissa niin kauan kuin tiedostoja on ollut olemassa. Tiedostonlukutoimintojen kehitys on kulkenut rinnakkain muun IT-teollisuuden kanssa.
2. Vaihtoehtoja: TypeScriptin lisäksi on olemassa monia muita ohjelmointikieliä, jotka tukevat tiedoston lukemista, kuten Java, Python ja C++. Jotkin näistä kielistä tarjoavat jopa useita tapoja tiedoston lukemiseen.
3. Toteutuksen yksityiskohdat: `fs`-moduulin `readFile`-funktio käyttää kahden tapahtuman mallia: se aloittaa tiedoston lukemisen ja kutsuu funktion joko virheen tai onnistuneen lukemisen jälkeen. Tämä on esimerkki JavaScriptin ja TypeScriptin tapahtumavetoisesta suunnittelusta.

## Lisätietoja
Lisätietoja tekstitiedostojen lukemisesta ja muista aiheeseen liittyvistä asioista löytyy seuraavista lähteistä:
- [Node.js Doc: File System](https://nodejs.org/api/fs.html)
- [Mozilla Developer Network: File and File Reader API](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications)
- [StackOverflow: Read file line by line in TypeScript](https://stackoverflow.com/questions/6156501/read-a-file-one-line-at-a-time-in-node-js)