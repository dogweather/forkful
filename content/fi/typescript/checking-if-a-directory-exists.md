---
title:                "TypeScript: Tarkistetaan, onko hakemisto olemassa."
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan TypeScript-ohjelmointiblogia! Tässä blogikirjoituksessa tarkastelemme, miksi ja miten tarkistaa, onko hakemisto olemassa TypeScriptissä. Tämä on tärkeä taito, jonka avulla voit välttyä virheiltä ja varmistaa, että ohjelmasi toimii oikein.

## Kuinka tehdä

```TypeScript

if (fs.existsSync(directoryPath)) {
    console.log("Hakemisto on olemassa");
} else {
    console.log("Hakemistoa ei ole olemassa");
}

```

Yllä oleva koodiesimerkki käyttää Node.js:n sisäänrakennettua "fs.existsSync()" -funktiota tarkistaakseen, onko annettua hakemistopolkua olemassa. Jos polku on olemassa, tulostetaan "Hakemisto on olemassa", muuten tulostetaan "Hakemistoa ei ole olemassa". Huomaa, että tämä esimerkki olettaa, että olet määrittänyt "fs" -moduulin käytettäväksi.

Voit myös käyttää "fs.existsSync()" yhdessä "path.join()" -funktion kanssa luodaksesi hakemistopolun. Alla olevassa esimerkissä luodaan ensin hakemistopolku "src/data", joka sitten tarkistetaan, onko se olemassa.

```TypeScript

const path = require('path');

const directory = path.join(__dirname, 'src', 'data'); // src/data
if (fs.existsSync(directory)) {
    console.log("Hakemisto on olemassa");
} else {
    console.log("Hakemistoa ei ole olemassa");
}

```

Molemmat esimerkit antavat saman tuloksen, jos kyseinen hakemisto on olemassa.

## Syvällisempi perehtyminen

Tietääksesi tarkalleen, miten "fs.existsSync()" toimii, on hyödyllistä tietää, miten hakemistot ja tiedostot tallennetaan tietokoneella. Hakemistojen ja tiedostojen polut tallennetaan hakemistopuuhun, jossa jokaisella hakemistolla on oma uniikki polkunsa.

Kun käytät "fs.existsSync()" -funktiota, se tarkistaa, onko annettu polku olemassa hakemistopuussa. Jos polku löytyy, tulos on "true", muuten "false". On myös huomionarvoista, että "fs.existsSync()" pystyy tarkistamaan vain olemassa olevan polun, eikä se luo uutta hakemistopolkua automaattisesti, jos sitä ei ole olemassa.

## Katso myös

- [Node.js 'fs' moduulin dokumentaatio](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [Node.js 'path' moduulin dokumentaatio](https://nodejs.org/dist/latest-v14.x/docs/api/path.html)