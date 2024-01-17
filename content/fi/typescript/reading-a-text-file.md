---
title:                "Tekstitiedoston lukeminen"
html_title:           "TypeScript: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lukeminen teksti tiedostoja on prosessi, jossa tietokone lukee ja tulkkaa tiedoston sisältöä. Tämä on tärkeä osa ohjelmointia, koska ohjelmat tarvitsevat usein pääsyä ulkoiseen tietoon, kuten käyttäjän antamiin tietoihin tai tallennettuihin tietokantoihin.

## Kuinka:

Esimerkki luodaksesi lukijan ja tulostamaan tiedoston sisällön:

```TypeScript
import * as fs from 'fs';

// Luodaan lukija
const reader = fs.createReadStream('tiedosto.txt');

// Käsitellään tiedoston sisältö
reader.on('data', (data) => {
    console.log(data.toString());
});

// Tulostetaan mahdolliset virheet
reader.on('error', (err) => {
    console.log(err);
});
```

Esimerkki tulostettavasta tiedostosta:

```
Tervetuloa lukemaan!
Tämä on esimerkki teksti tiedosto.
Täällä voit lukea koodia ja oppia uusia asioita.
```

## Syvempi sukellus:

Teksti tiedostojen lukeminen on ollut oleellinen osa ohjelmointia jo varhaisista ajoista lähtien. Aikaisemmin se tapahtui manuaalisesti, kun ohjelmoija lukisi tiedoston riveittäin ja tallentaisi tiedot muuttujiin käytettäväksi ohjelmassa. Nykyään on olemassa monia eri tapoja lukea teksti tiedostoja, kuten käyttämällä erilaisia kirjastoja tai moduuleja. Käyttäjien syöttämien tietojen validointi on myös tärkeä syy lukea teksti tiedostoja.

## Katso myös:

- [Node.js dokumentaatio](https://nodejs.org/docs/latest/api/fs.html)
- [Typescript kirjasto](https://www.typescriptlang.org/docs/handbook/fs.html)