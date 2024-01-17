---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "TypeScript: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstin kirjoittaminen on yksi tärkeä osa ohjelmoinnissa, joka mahdollistaa ohjelman tallentaa tai lukea tietoa ulkoisesta tiedostosta. Tämä voi olla esimerkiksi tekstitiedosto, HTML-tiedosto tai CSV-tiedosto. Tekstien kirjoittamista tarvitaan esimerkiksi tietokantojen käsittelyssä tai käyttäjän antaman datan tallentamisessa.

## Miten:

Käytä TypeScriptiä kirjoittamaan tekstitiedostoja käyttämällä sisäänrakennettua ```fs```-kirjastoa. Kirjoita tiedoston nimi, mitä haluat sen sisältävän ja antaa tarvittavat käskyt tallentamiseen esimerkiksi käyttäen ```writeFile()```-metodia. Tämän jälkeen voit lukea tiedoston sisällön käyttämällä ```readFile()```-metodia.

```
import fs from 'fs';

// Kirjoita tiedoston nimi ja sisältö
fs.writeFile('tekstifile.txt', 'Tervehdys maailma!');

// Lue tiedoston sisältö
fs.readFile('tekstifile.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data); // Tulostaa tekstifile.tx:n sisällön
});
```

## Syväsukellus:

Tekstin kirjoittaminen on vanha tekniikka, jota käytetään edelleen monissa ohjelmoinnin osa-alueissa. Lisäksi on olemassa myös muita tapoja tallentaa ja lukea tietoa, kuten käyttämällä tietokantoja. Tekstien kirjoittamisessa on hyvä muistaa myös koodin turvallisuus ja validointi, jotta vältytään mahdollisilta tietoturvariskeiltä.

## Katso myös:

[Node.js dokumentaatio fs-moduulista](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback)

[TypeScript oppaat ja tiedot](https://www.typescriptlang.org/docs/home.html)