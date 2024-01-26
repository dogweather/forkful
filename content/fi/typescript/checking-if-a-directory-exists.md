---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:58:48.197909-07:00
html_title:           "Gleam: Onko hakemisto olemassa? Tarkistaminen"
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tarkistamme, onko hakemisto olemassa, ettei sovelluksemme kaadu yrittäessään lukea tai kirjoittaa olemattomaan kansioon. Tämä on tärkeää tiedostojärjestelmän virheiden ehkäisyssä ja tiedonhallinnassa.

## Kuinka:
```TypeScript
import * as fs from 'fs';
import { promisify } from 'util';

const exists = promisify(fs.exists);

async function checkDirectory(directoryPath: string) {
  try {
    const dirExists = await exists(directoryPath);
    console.log(dirExists ? 'Hakemisto löytyy.' : 'Hakemistoa ei ole olemassa.');
  } catch (error) {
    console.error('Virhe tarkistaessa hakemistoa:', error);
  }
}

// Käytä funktiota
checkDirectory('./polku/hakemistoon');
```
Esimerkkilokissa näkyy joko "Hakemisto löytyy." tai "Hakemistoa ei ole olemassa." sen mukaan, onko hakemisto olemassa.

## Syväsukellus

Hakemistojen olemassaolon tarkistus on ollut osa ohjelmistokehitystä tiedostojärjestelmien alkuajoista asti. JavaScript-ympäristössä, kuten Node.js:ssä, tämä tehdään `fs` (FileSystem)-moduulin avulla, joka tarjoaa synkronisia ja asynkronisia funktioita tähän tarkoitukseen.

Historiallisesti `fs.exists` oli tapa tarkistaa kansioita, mutta se on virallisesti katsottu vanhentuneeksi, koska se ei laske virheitä normaaliin Node.js callback -malliin. Nykyään `fs.access` on suositeltavampi tapa, koska se seuraa Node.js:n paradigmaa ja antaa mahdollisuuden tarkistaa myös käyttöoikeuksia.

Kuitenkin, kun käytetään TypeScriptiä, joka on JavaScriptin superset, tulee mukana tyypit ja kehittyneemmät abstraktiot virhekäsittelyyn ja asynkronisiin operaatioihin, kuten yllä olevassa esimerkissä nähtiin.

Eri lähestymistapoina voidaan mainita myös käyttö funktioiden `fs.stat` tai `fs.readdir` kautta, jotka antavat enemmän tietoa tiedostojärjestelmästä. Nämä funktiot voivat heittää virheen, jos polku ei ole olemassa, joten ne vaativat virheiden käsittelyä.

## Katso Myös
- Node.js FileSystem Documentation: [Node.js fs](https://nodejs.org/api/fs.html)
- TypeScript Documentation: [TypeScript Lang](https://www.typescriptlang.org/docs/)
- More on Promises and async/await pattern: [MDN - Using Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises)
