---
title:                "Javascript: Tarkistetaan löytyykö hakemistoa"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoijat tarvitsevat tarkistaa, onko tietty hakemisto olemassa ennen kuin he jatkavat sovelluksensa suorittamista. Tämä voi estää virheitä ja auttaa varmistamaan, että ohjelma toimii oikein. Tässä blogikirjoituksessa käsittelemme, miten tarkistaa, onko hakemisto olemassa käyttäen Javascriptiä.

## Kuinka tehdä

Voit tarkistaa, onko hakemisto olemassa käyttäen "fs" -moduulia Javascriptissä. Ensimmäinen vaihe on tuoda tämä moduuli ohjelmaan "require" -toiminnolla. Sitten voit käyttää "existsSync" -toimintoa tarkistaaksesi halutun hakemiston olemassaolon.

```Javascript
const fs = require('fs');
if (fs.existsSync('/path/to/directory')) {
   console.log('Hakemisto on olemassa');
} else {
   console.log('Hakemistoa ei ole olemassa');
}
```

Tässä esimerkissä käytämme ehdollista lausetta tarkistamaan, onko hakemisto olemassa ja tulostamme sen mukaisen viestin. Voit myös tallentaa tuloksen muuttujaan ja käyttää sitä jatkotoimenpiteissä.

```Javascript
const fs = require('fs');
const directoryExists = fs.existsSync('/path/to/directory');
if (directoryExists) {
   // tee jotain hakemistolle
} else {
   // luo hakemisto
}
console.log(directoryExists); // tulostaa true tai false
```

## Syventävä sukellus

"fs" -moduulin "existsSync" -toiminto palauttaa boolean-arvon riippuen siitä, onko hakemisto olemassa vai ei. Jos haluat tarkistaa tiedoston sijasta hakemistoa, voit käyttää "statSync" -toimintoa, joka palauttaa tiedostojen ja hakemistojen tiedot. Tämän tiedon perusteella voit tarkistaa, onko kohde kansio tai tiedosto.

```Javascript
const fs = require('fs');
const stats = fs.statSync('/path/to/directory');
if (stats.isDirectory()) {
   console.log('Kohde on hakemisto');
} else {
   console.log('Kohde ei ole hakemisto');
}
```

On myös hyvä huomioida, että "existsSync" ja "statSync" -toiminnot ovat synkronisia, mikä tarkoittaa, että ne suoritetaan ennen kuin ohjelma jatkaa muihin toimenpiteisiin. Jos haluat tarkistaa hakemiston olemassaolon asynkronisesti, voit käyttää "exists" -toimintoa, joka hyväksyy kolmannen argumentin, joka on callback-funktio.

```Javascript
const fs = require('fs');
fs.exists('/path/to/directory', (exists) => {
   if (exists) {
      console.log('Hakemisto on olemassa');
   } else {
      console.log('Hakemistoa ei ole olemassa');
   }
});
```

## Katso myös

- [Node.js fs moduuli](https://nodejs.org/api/fs.html)
- [fs.existsSync dokumentaatio](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Node.js asynkroninen ohjelmointi](https://nodejs.dev/learn/understanding-the-nodejs-event-loop)