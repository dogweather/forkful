---
title:                "Arbeta med CSV"
aliases: - /sv/typescript/working-with-csv.md
date:                  2024-02-03T19:21:28.607955-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med CSV (Comma-Separated Values) innebär att läsa från och skriva till CSV-filer, ett vanligt datautbytesformat som används på grund av sin enkelhet och breda stöd över olika plattformar och språk. Programmerare engagerar sig med CSV-filer för att importera eller exportera data från applikationer, databaser och tjänster, vilket möjliggör enkel datahantering och delning.

## Hur man gör:

I TypeScript kan du arbeta med CSV-filer genom inbyggd kod eller genom att utnyttja tredjepartsbibliotek som `csv-parser` för att läsa och `csv-writer` för att skriva CSV-filer.

### Att läsa CSV med `csv-parser`

Först, installera `csv-parser` via npm:

```
npm install csv-parser
```

Läs sedan en CSV-fil så här:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Utdata: En array av objekt, varje representerar en rad i CSV-filen
  });
```

Med antagandet att `data.csv` innehåller:

```
name,age
Alice,30
Bob,25
```

Skulle utdatan vara:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### Att skriva CSV med `csv-writer`

För att skriva till en CSV-fil, installera först `csv-writer`:

```
npm install csv-writer
```

Använd sedan det så här:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NAMN'},
    {id: 'age', title: 'ÅLDER'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('CSV-filen har skrivits framgångsrikt'));
```

Denna kod skriver följande till `out.csv`:

```
NAMN,ÅLDER
Alice,30
Bob,25
```

Dessa exempel visar hur du integrerar CSV-behandling i dina TypeScript-projekt effektivt, oavsett om det handlar om att läsa data för analys eller att bevara applikationsdata externt.
