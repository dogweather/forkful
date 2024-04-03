---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:33.942683-07:00
description: "Come fare: In TypeScript, puoi lavorare con i file CSV tramite codice\
  \ nativo o sfruttando librerie di terze parti come `csv-parser` per la lettura e\
  \ `csv-\u2026"
lastmod: '2024-03-13T22:44:43.198648-06:00'
model: gpt-4-0125-preview
summary: In TypeScript, puoi lavorare con i file CSV tramite codice nativo o sfruttando
  librerie di terze parti come `csv-parser` per la lettura e `csv-writer` per la scrittura
  di file CSV.
title: Lavorare con i CSV
weight: 37
---

## Come fare:
In TypeScript, puoi lavorare con i file CSV tramite codice nativo o sfruttando librerie di terze parti come `csv-parser` per la lettura e `csv-writer` per la scrittura di file CSV.

### Leggere CSV con `csv-parser`
Prima di tutto, installa `csv-parser` tramite npm:

```
npm install csv-parser
```

Poi, leggi un file CSV in questo modo:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Output: Array di oggetti, ciascuno rappresentante una riga nel CSV
  });
```

Assumendo che `data.csv` contenga:

```
name,age
Alice,30
Bob,25
```

L'output sarà:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### Scrivere CSV con `csv-writer`
Per scrivere su un file CSV, prima installa `csv-writer`:

```
npm install csv-writer
```

Poi, utilizzalo così:

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
  .then(() => console.log('Il file CSV è stato scritto con successo'));
```

Questo codice scrive quanto segue in `out.csv`:

```
NAME,AGE
Alice,30
Bob,25
```

Questi esempi mostrano come integrare l'elaborazione dei file CSV nei tuoi progetti TypeScript in modo efficiente, sia che si tratti di leggere dati per l'analisi o di persistere i dati dell'applicazione esternamente.
