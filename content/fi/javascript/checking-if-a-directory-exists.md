---
title:    "Javascript: Tarkista onko hakemisto olemassa."
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Miksi tarkistaa, onko hakemisto olemassa?

Hakemistojen tarkistaminen on tärkeä osa ohjelmistokehitystä, sillä se auttaa varmistamaan, että ohjelma toimii ilman virheitä ja keskeytyksiä. Tarkasteltaessa, onko hakemisto olemassa, voimme varmistaa, että ohjelma voi saada tarvittavat tiedostot ja käsitellä niitä oikein.

# Miten tarkistaa, onko hakemisto olemassa?

Hakemiston olemassaolon tarkistaminen voidaan tehdä käyttämällä JavaScriptin `fs`-moduulia, joka sisältää toimintoja tiedostojen ja hakemistojen hallintaan. Käytämme `fs.existsSync()` -metodia, joka tarkistaa, onko annetulle polulle olemassa oleva tiedosto tai hakemisto.

Esimerkiksi, jos haluamme tarkistaa, onko hakemisto nimeltä "kuvat" olemassa, voimme käyttää seuraavaa koodia:

```Javascript
const fs = require("fs");
const polku = "./kuvat";

if (fs.existsSync(polku)) {
  console.log("Hakemisto on olemassa.");
} else {
  console.log("Hakemistoa ei ole olemassa.");
}
```

Tässä koodissa käytämme `fs.existsSync()` -metodia, joka palauttaa `true`, jos hakemisto on olemassa, ja `false`, jos sitä ei ole olemassa. Tämän seurauksena tulostamme joko "Hakemisto on olemassa." tai "Hakemistoa ei ole olemassa." riippuen tuloksen arvosta.

# Syvempi sukellus

On tärkeää huomata, että `fs.existsSync()` -metodi toimii vain synkronisesti, mikä tarkoittaa, että se ei palauta tuloksia ennen kuin tarkistus on tehty. Tämä voi aiheuttaa suorituskykyongelmia, jos tarkistamme useita tiedostoja ja hakemistoja. Tässä tapauksessa on parempi käyttää `fs.stat()` -metodia, joka toimii asynkronisesti ja palauttaa tiedon hakemiston olemassaolosta tekemättä suorituskykymontaa. Lisäksi `fs.existsSync()` -metodi voi aiheuttaa virheitä, jos käytämme nykyaikaisempia kirjastoja, koska se ei tue asynkronisia toimintoja.

# Katso myös

- [fs-moduulin dokumentaatio](https://nodejs.org/api/fs.html)
- [fs.stat()-metodin dokumentaatio](https://nodejs.org/api/fs.html#fs_fs_stat_path_options_callback)
- [Kuvien haku hakemistosta käyttämällä fs-moduulia](https://www.javascripttutorial.net/node-js/node-js-read-files-in-directory/)