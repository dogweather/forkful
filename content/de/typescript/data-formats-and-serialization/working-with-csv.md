---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:27.997360-07:00
description: "Wie: In TypeScript k\xF6nnen Sie mit CSV-Dateien durch nativen Code\
  \ oder durch die Nutzung von Drittanbieter-Bibliotheken wie `csv-parser` f\xFCr\
  \ das Lesen und\u2026"
lastmod: '2024-03-13T22:44:53.652852-06:00'
model: gpt-4-0125-preview
summary: "In TypeScript k\xF6nnen Sie mit CSV-Dateien durch nativen Code oder durch\
  \ die Nutzung von Drittanbieter-Bibliotheken wie `csv-parser` f\xFCr das Lesen und\
  \ `csv-writer` f\xFCr das Schreiben von CSV-Dateien arbeiten."
title: Arbeiten mit CSV
weight: 37
---

## Wie:
In TypeScript können Sie mit CSV-Dateien durch nativen Code oder durch die Nutzung von Drittanbieter-Bibliotheken wie `csv-parser` für das Lesen und `csv-writer` für das Schreiben von CSV-Dateien arbeiten.

### CSV lesen mit `csv-parser`
Zuerst installieren Sie `csv-parser` via npm:

```
npm install csv-parser
```

Lesen Sie dann eine CSV-Datei wie folgt:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Ausgabe: Array von Objekten, jedes repräsentiert eine Zeile in der CSV
  });
```

Angenommen, `data.csv` enthält:

```
name,age
Alice,30
Bob,25
```

Die Ausgabe wird sein:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### CSV schreiben mit `csv-writer`
Um in eine CSV-Datei zu schreiben, installieren Sie zuerst `csv-writer`:

```
npm install csv-writer
```

Verwenden Sie es dann wie folgt:

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NAME'},
    {id: 'age', title: 'AGE'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('Die CSV-Datei wurde erfolgreich geschrieben'));
```

Dieser Code schreibt folgendes in `out.csv`:

```
NAME,AGE
Alice,30
Bob,25
```

Diese Beispiele zeigen, wie Sie die CSV-Verarbeitung in Ihren TypeScript-Projekten effizient integrieren können, egal ob es um das Lesen von Daten für die Analyse geht oder um das externes Speichern von Anwendungsdaten.
