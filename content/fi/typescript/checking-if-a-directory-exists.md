---
title:    "TypeScript: Tarkistaako hakemisto on olemassa"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi
Tiedostonhallinnan ylläpito ja organisoiminen on olennainen osa ohjelmointia. Usein meidän täytyy tarkistaa, onko haluttua hakemistoa olemassa ja ohjata sen perusteella koodiamme eteenpäin.

## Miten
```TypeScript
import fs from 'fs';

// Tarkistetaan onko hakemisto olemassa

if (fs.existsSync("./hakemisto")) {
  // Tee tarvittavat toimenpiteet
  console.log("Hakemisto on olemassa");
} else {
  console.log("Hakemistoa ei löytynyt");
}

```
Tämä esimerkkikoodi käyttää Node.js:n fs-moduulia tarkistaakseen, onko hakemisto olemassa. Käytämme existSync-funktiota, joka palauttaa totuusarvon sen mukaan, löytyykö annetulla polulla oleva hakemisto vai ei. Sen perusteella voimme tehdä haluamamme toimenpiteet ohjelmassamme.

## Syvempi sukellus
Fs-moduuli tarjoaa myös muita vaihtoehtoja tarkistaa hakemiston olemassaolo, kuten accessSync-funktion, joka tarkistaa, onko hakemistoon pääsyä tai lukuoikeutta. Voimme myös käyttää exists-metodia, joka palauttaa Promisen ja on näin ollen käyttökelpoinen asynkronisessa koodissa. Lisää tietoa fs-moduulista ja sen metodeista löydät [Node.js:n dokumentaatiosta](https://nodejs.org/api/fs.html).

## Katso myös
- [fs-moduuli Node.js:n dokumentaatiossa](https://nodejs.org/api/fs.html)
- [Node.js - hakemistojen hallinta ja käsittely](https://nodejs.org/en/knowledge/file-system/how-to-write-files-in-nodejs/)
- [TypeScriptin perusteet](https://www.typescriptlang.org/docs/handbook/basic-types.html)