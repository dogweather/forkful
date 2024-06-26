---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:19.321514-07:00
description: 'Hoe te: Het lezen van CSV in TypeScript is eenvoudig met bibliotheken
  zoals `papaparse`. Om CSV-bestanden te kunnen behandelen, installeer het eerst.'
lastmod: '2024-03-13T22:44:50.573858-06:00'
model: gpt-4-0125-preview
summary: Het lezen van CSV in TypeScript is eenvoudig met bibliotheken zoals `papaparse`.
title: Werken met CSV
weight: 37
---

## Hoe te:
Het lezen van CSV in TypeScript is eenvoudig met bibliotheken zoals `papaparse`. Om CSV-bestanden te kunnen behandelen, installeer het eerst:

```bash
npm install papaparse
```

Zo lees je een CSV-bestand:

```typescript
import * as fs from 'fs';
import * as Papa from 'papaparse';

const csvFilePath = 'pad/naar/jouw/bestand.csv';
const fileContent = fs.readFileSync(csvFilePath, 'utf8');

Papa.parse(fileContent, {
  complete: (resultaat) => {
    console.log(resultaat.data);
  }
});
```

Om CSV te schrijven, zou je `csv-writer` kunnen gebruiken. Installeer het met:

```bash
npm install csv-writer
```

En schrijf dan naar een CSV-bestand als volgt:

```typescript
import * as createCsvWriter from 'csv-writer';

const csvWriter = createCsvWriter.createObjectCsvWriter({
  path: 'pad/naar/jouw/uitvoer.csv',
  kopteksten: [
    {id: 'naam', titel: 'NAAM'},
    {id: 'leeftijd', titel: 'LEEFTIJD'}
  ]
});

const gegevens = [
  { naam: 'John', leeftijd: 28 },
  { naam: 'Jane', leeftijd: 32 }
];

csvWriter.writeRecords(gegevens)
  .then(() => console.log('Gegevens succesvol naar CSV-bestand geschreven.'));
```

De uitvoer in 'uitvoer.csv' zal zijn:

```
NAAM,LEEFTIJD
John,28
Jane,32
```

## Diepgaand
CSV is een basis in gegevensuitwisseling sinds het vroege computertijdperk vanwege de leesbaarheid en eenvoud. Het is niet zonder problemen; bijvoorbeeld, het gebrek aan standaardisatie kan leiden tot verwerkingsfouten. Alternatieven zoals JSON en XML bieden meer complexe structuren en datatypes. Bij het implementeren van CSV parsers/schrijvers, overweeg de karaktercodering en correcte afhandeling van speciale karakters om bugs te vermijden.

## Zie Ook
- De `papaparse` documentatie: [Papa Parse - Krachtige CSV Parser](https://www.papaparse.com/)
- De `csv-writer` documentatie: [CSV Writer - CSV-bestandschrijver voor Node](https://csv.js.org/)
- Voor een dieper technisch begrip biedt het document RFC 4180 de de facto standaard voor CSV-formaten: [RFC 4180](https://tools.ietf.org/html/rfc4180)
- Voor een vergelijking van bestandsformaten, zie: [JSON vs XML vs CSV](https://www.geeksforgeeks.org/difference-between-json-and-xml/)
