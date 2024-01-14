---
title:    "Javascript: Tarkistetaan, onko hakemistoa olemassa"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Monilla verkkosivustoilla ja sovelluksilla on tarve tarkistaa, onko hakemisto olemassa ennen kuin se voidaan käyttää. Tämä on erityisen tärkeää, jos halutaan tallentaa tai lukea tietoja tietyssä hakemistossa tai varmistaa, että tietyt tiedostot ovat saatavilla.

## Miten

Jotta voit tarkistaa, onko hakemisto olemassa Javascriptillä, voit käyttää Node.js:ää ja sen integroituja fs (File System) -moduuleita.

```javascript
const fs = require('fs');
const directory = './hakemisto';

// Tarkistetaan, onko hakemisto olemassa
if (fs.existsSync(directory)) {
    console.log("Hakemisto on olemassa.");
} else {
    console.log("Hakemistoa ei löytynyt.");
}

```

Koodi alkaa tuomalla tarvittavan fs-moduulin ja määrittelemällä tarkistettavan hakemiston polun. Sitten if-else-lauseessa käytetään fs.existsSync () -funktiota tarkistamaan, onko hakemisto olemassa. Jos kyseinen hakemisto löytyy, tulostetaan viesti "Hakemisto on olemassa", muuten tulostetaan viesti "Hakemistoa ei löytynyt".

Voit myös käyttää fs.statSync () -funktiota tarkistaaksesi, onko kyseinen polku hakemisto vai tiedosto. Tämä voi olla hyödyllistä, jos haluat määrittää tarkemmin, mitä toimintoja haluat suorittaa hakemistossa. Esimerkiksi jos kyseessä on tiedosto, saatat haluta lukea sen sisältöä, mutta jos kyseessä on hakemisto, saatat haluta luoda uuden tiedoston tai lisätä siihen tietoja.

```javascript
const fs = require('fs');
const path = './tiedosto.txt';

// Tarkistetaan, onko path hakemisto vai tiedosto
if (fs.statSync(path).isDirectory()) {
    console.log("Kyseessä on hakemisto.");
} else {
    console.log("Kyseessä on tiedosto.");
}

```

## Syventyvä tarkastelu

Tarkistettaessa, onko hakemisto olemassa, on tärkeää muistaa, että Javascriptin fs-moduulit käyttävät Node.js:n käyttöjärjestelmän alaista tiedostojärjestelmää eikä selaimen ympäristössä. Tämä tarkoittaa, että hakemistojen tarkistaminen on mahdollista vain Node-sovelluksissa, eikä se toimi selaimen Javascript-sovelluksissa.

Samoin on tärkeää muistaa, että tarkistettaessa hakemiston olemassaoloa käytetään polkua, joka voi olla suhteellinen tai absoluuttinen. Suhteellinen polku viittaa nykyiseen sijaintiin, kun taas absoluuttinen polku viittaa hakemiston täydelliseen polkuun, esimerkiksi "/hakemistot/hakemisto1/hakemisto2".

## Katso myös

- [Node.js - virallinen verkkosivusto](https://nodejs.org/fi/)
- [fs - Node.js Documentation](https://nodejs.dev/learn/the-nodejs-fs-module)
- [Node.js - File System Module](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)