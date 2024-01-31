---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma-Separated Values) on yksinkertainen tiedostomuoto, jolla tallennetaan taulukollista dataa. Ohjelmoijat käyttävät sitä helposti luettavan ja kirjoitettavan rakenteen sekä yleisen tietojen vaihdon soveltuvuuden vuoksi.

## How to:
Työskentele CSV:n kanssa TypeScriptillä `csv-parse` ja `csv-stringify` kirjastojen avulla. Lue, muokkaa ja kirjoita tiedostoja.

```TypeScript
import { parse } from 'csv-parse';
import { stringify } from 'csv-stringify';

// CSV:n lukeminen ja parsiminen
const inputCSV = 'nimi,ikä\nJari,30\nLeena,25';
parse(inputCSV, {
  columns: true,
  delimiter: ','
}, function(err, output) {
  console.log(output);
});

// CSV:n luominen ja muotoilu
const records = [{ nimi: 'Jari', ikä: 30 }, { nimi: 'Leena', ikä: 25 }];
stringify(records, {
  header: true,
  columns: ['nimi', 'ikä']
}, function(err, output) {
  console.log(output);
});
```

Tulosteet:
1. `[ { nimi: 'Jari', ikä: '30' }, { nimi: 'Leena', ikä: '25' } ]`
2. `nimi,ikä\nJari,30\nLeena,25\n`

## Deep Dive
CSV:n juuret ovat varhaisessa tietokoneiden käytössä, jolloin yksinkertaiset tekstitiedostot olivat pääasiallinen tapa tallentaa ja vaihtaa tietoa. JSON ja XML ovat moderneja vaihtoehtoja CSV:lle, mutta ne eivät ole yhtä kevyitä eivätkä niitä käsitellä yhtä tehokkaasti suurilla datamäärillä. TypeScript toteuttaminen perustuu Node.js-pohjaiseen lukijaan ja kirjoittajaan, joita voi optimoida muuntamaan suuria tietomassoja asynkronisella koodilla.

## See Also
CSV:tä käsitteleviä TypeScript resursseja:
- `csv-parse` dokumentaatio: [https://csv.js.org/parse/](https://csv.js.org/parse/)
- `csv-stringify` dokumentaatio: [https://csv.js.org/stringify/](https://csv.js.org/stringify/)
- Node.js virallinen dokumentaatio virran käsittelyyn: [https://nodejs.org/api/stream.html](https://nodejs.org/api/stream.html)
- MDN Web Docs CSV:stä ja sen käytöstä JavaScriptillä: [https://developer.mozilla.org/docs/Web/JavaScript/Data_structures#csv](https://developer.mozilla.org/docs/Web/JavaScript/Data_structures#csv)
