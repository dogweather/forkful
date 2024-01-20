---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Lua: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# TypeScript: Tarkista onko hakemisto olemassa

## Mitä & Miksi?

Hakemiston olemassaolon tarkistaminen tarkoittaa tiedostojärjestelmässä sijaitsevan hakemiston löytämistä ja sen varmistamista, että se on olemassa. Ohjelmoijat tekevät tätän tarkistaakseen etukäteen ovatko resurssit saatavana, ennen kuin he aloittavat työskentelyn niihin.

## Kuinka tehdä:

Tarkistamme onko hakemisto olemassa käyttämällä Node.js 'fs' moduulia. 

```TypeScript
import { existsSync } from 'fs';

function checkDirectory(directoryPath: string): void {
  if (existsSync(directoryPath)) {
    console.log("Hakemisto on olemassa.");
  } else {
    console.log("Hakemisto ei ole olemassa.");
  }
}

checkDirectory("./someDirectory");  // Esimerkkihakemiston tarkistus
```

Käytettäessä 'fs' moduulia, `existsSync`-funktio tarkistaa, onko tietty hakemisto olemassa tiedostojärjestelmässä. 

## Syvällisempi sukellus:

Hakemistojen olemassaolon tarkistaminen on joukkotiedostojärjestelmien perusominaisuuksia. Siitä lähtien, kun hakemistorakenteet otettiin käyttöön, ohjelmoijat ovat tarvinneet keinoja tarkistaa, ovatko heidän osoittamansa tiedostopolut olemassa.

Vaihtoehtoisesti voit käyttää `fs.stat` tai `fs.access` funktioita. Mutta näitä funktioita on kutsuttava virheenkäsittelyn kera, koska ne heittävät virheen, jos hakemistoa ei ole olemassa.

Kun tarkistat onko hakemisto olemassa, yrität lukea tiedostojärjestelmän metatietoja. Tämä ei vaadi tiedostojen sisällön lukemista, joten se on suhteellisen nopea toiminto.

## Katso myös:

1. Node.js 'fs' moduuli dokumentaatio: https://nodejs.org/api/fs.html
2. TypeScriptin opas: https://www.typescriptlang.org/docs/handbook/intro.html
3. Hakemistorakenteen Wikipedia-artikkeli: https://fi.wikipedia.org/wiki/Hakemisto_(tietotekniikka)