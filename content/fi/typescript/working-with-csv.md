---
title:                "Työskentely csv:n kanssa"
html_title:           "TypeScript: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
CSV on lyhenne sanoista "comma-separated values" eli pilkuin erotetut arvot. Se on yksi tapa tallentaa ja järjestää taulukkodataa tekstitiedostoon. Ohjelmoijat käyttävät CSV-tiedostoja usein datan tallentamiseen ja jakamiseen eri ohjelmien välillä.

## Miten:
```TypeScript
// Tuodaan CSV-tiedosto käyttäen npm-pakettia "csvtojson"
import { csv } from 'csvtojson';

// Luodaan muuttuja, johon tallennetaan tuodun tiedoston sisältö
const data = await csv().fromFile('data.csv');

// Tulostetaan tiedoston sisältö konsoliin
console.log(data);

// Esimerkki CSV-tiedoston sisällöstä:
/*
nimi, ikä, suosikki_väri
Matti, 25, sininen
Anna, 30, punainen
*/

// Tuloste:
/*
[
  { nimi: 'Matti', ikä: 25, suosikki_väri: 'sininen' },
  { nimi: 'Anna', ikä: 30, suosikki_väri: 'punainen' }
]
*/
```

## Syvällinen sukellus:
CSV-muoto kehitettiin 1970-luvulla IBM:n käyttöön, mutta se on sittemmin yleistynyt myös muilla alustoilla. CSV ei ole ainoa tapa tallentaa taulukkodataa tekstitiedostoon, mutta se on yksi yleisimmin käytetyistä. Muita vaihtoehtoja ovat esimerkiksi XML, JSON ja YAML. CSV-tiedoston käsittelyyn on olemassa useita erilaisia npm-paketteja ja TypeScript tarjoaa myös omia sisäänrakennettuja työkaluja kuten ```fs```-moduulin.

## Katso myös:
- [CSV-tiedoston muoto Wikipedia:ssa](https://fi.wikipedia.org/wiki/CSV-tiedoston_muoto)
- [csvtojson npm-paketti](https://npmjs.com/package/csvtojson)
- [TypeScriptin ```fs```-moduuli](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)