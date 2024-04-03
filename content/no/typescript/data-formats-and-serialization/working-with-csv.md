---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:24.238177-07:00
description: "\xC5 jobbe med CSV (Comma-Separated Values) inneb\xE6rer \xE5 lese fra\
  \ og skrive til CSV-filer, et vanlig datautvekslingsformat som brukes p\xE5 grunn\
  \ av sin enkelhet\u2026"
lastmod: '2024-03-13T22:44:40.554200-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med CSV (Comma-Separated Values) inneb\xE6rer \xE5 lese fra og\
  \ skrive til CSV-filer, et vanlig datautvekslingsformat som brukes p\xE5 grunn av\
  \ sin enkelhet og brede st\xF8tte p\xE5 tvers av ulike plattformer og spr\xE5k."
title: Arbeide med CSV
weight: 37
---

## Hvordan:
I TypeScript kan du jobbe med CSV-filer gjennom nativ kode eller ved å benytte tredjepartsbiblioteker som `csv-parser` for lesing og `csv-writer` for skriving av CSV-filer.

### Lese CSV med `csv-parser`
Først, installer `csv-parser` via npm:

```
npm install csv-parser
```

Deretter, les en CSV-fil slik:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Utdata: Et array av objekter, hver representerer en rad i CSV-filen
  });
```

Forutsetter at `data.csv` inneholder:

```
name,age
Alice,30
Bob,25
```

Vil utdata være:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### Skrive CSV med `csv-writer`
For å skrive til en CSV-fil, installer først `csv-writer`:

```
npm install csv-writer
```

Deretter bruk den slik:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NAVN'},
    {id: 'age', title: 'ALDER'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('CSV-filen ble skrevet vellykket'));
```

Denne koden skriver følgende til `out.csv`:

```
NAVN,ALDER
Alice,30
Bob,25
```

Disse eksemplene viser hvordan du effektivt kan integrere CSV-behandling i dine TypeScript-prosjekter, enten det er å lese data for analyse eller å bevare applikasjonsdata eksternt.
