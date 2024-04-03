---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:32.185715-07:00
description: "CSV:n (pilkulla erotetut arvot) k\xE4sittelyyn kuuluu CSV-tiedostojen\
  \ lukemista ja niihin kirjoittamista, mik\xE4 on yleinen tiedonvaihtomuoto sen\u2026"
lastmod: '2024-03-13T22:44:56.337448-06:00'
model: gpt-4-0125-preview
summary: "CSV:n (pilkulla erotetut arvot) k\xE4sittelyyn kuuluu CSV-tiedostojen lukemista\
  \ ja niihin kirjoittamista, mik\xE4 on yleinen tiedonvaihtomuoto sen yksinkertaisuuden\
  \ ja laajan tuen ansiosta eri alustoilla ja kielill\xE4."
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Kuinka:
TypeScriptissä voit työskennellä CSV-tiedostojen kanssa natiivikoodin tai kolmansien osapuolten kirjastojen, kuten `csv-parser` lukemiseen ja `csv-writer` kirjoittamiseen, avulla.

### CSV:n lukeminen `csv-parser` avulla
Asenna ensin `csv-parser` npm:n kautta:

```
npm install csv-parser
```

Lue sitten CSV-tiedosto näin:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Tuloste: Olioiden taulukko, kukin edustaa yhtä CSV:n riviä
  });
```

Olettaen, että `data.csv` sisältää:

```
name,age
Alice,30
Bob,25
```

Tuloste on:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### CSV:n kirjoittaminen `csv-writer` avulla
CSV-tiedostoon kirjoittaaksesi asenna ensin `csv-writer`:

```
npm install csv-writer
```

Käytä sitä sitten seuraavasti:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NIMI'},
    {id: 'age', title: 'IKÄ'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('CSV-tiedosto kirjoitettiin onnistuneesti'));
```

Tämä koodi kirjoittaa seuraavaa `out.csv`-tiedostoon:

```
NIMI,IKÄ
Alice,30
Bob,25
```

Nämä esimerkit näyttävät, kuinka integroida CSV:n käsittely tehokkaasti TypeScript-projekteihisi, oli kyseessä sitten tiedon lukeminen analyysiä varten tai sovellustiedon tallentaminen ulkoisesti.
