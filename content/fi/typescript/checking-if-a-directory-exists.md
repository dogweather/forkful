---
title:    "TypeScript: Tarkistetaan, onko hakemistoa olemassa"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

"# Miksi

Monissa ohjelmoinnin projekteissa on tarpeen tarkistaa, onko tietty hakemisto olemassa ja käsitellä sitä sen mukaan. Tämä yksinkertainen tehtävä voi säästää paljon aikaa ja estää virheitä jatkossa.

## Miten

Tarkistaaksesi, onko hakemisto olemassa TypeScriptissä, voit käyttää Node.js:n `fs`-moduulia. `fs` tarjoaa erilaisia toimintoja tiedostojen ja hakemistojen käsittelyyn.

```
TypeScript
import * as fs from 'fs';

// Tarkistetaan, onko hakemisto "photos" olemassa
if (fs.existsSync('./photos')) {
  console.log('Hakemisto on olemassa.');
} else {
  console.log('Hakemistoa ei löydy.');
}
```

Tämä koodi käyttää `existsSync()`-funktiota tarkistamaan, onko annetun polun mukainen tiedosto tai hakemisto olemassa. Jos hakemisto löytyy, konsoliin tulostetaan viesti "Hakemisto on olemassa". Jos hakemistoa ei löydy, tulostetaan "Hakemistoa ei löydy".

## Syventävä tieto

`existsSync()`-funktio palauttaa boolean-tyyppisen arvon, joten voit myös käyttää sitä ehtolauseissa ja tehdä haluamiasi toimenpiteitä sen perusteella, onko hakemisto olemassa vai ei. Voit myös käyttää `fs.statSync()`-funktiota saadaksesi tarkempia tietoja tiedostosta tai hakemistosta, kuten sen koon ja luomisajankohdan.

On myös hyvä huomioida, että `fs.existsSync()` ja `fs.statSync()` toimivat vain synkronisesti, joten ne voivat hidastaa sovelluksen suoritusta, jos käsiteltävien tiedostojen määrä on suuri.

# Katso myös

- [Node.js: fs-moduuli](https://nodejs.org/api/fs.html)
- [TypeScript for Beginners - Working with Files & Directories](https://youtu.be/F2uE_PvZJu0)
- [Managing files and directories in TypeScript using the Node.js File System Module](https://www.section.io/engineering-education/managing-files-and-directories-in-typescript-using-the-node-js-file-system-module/)