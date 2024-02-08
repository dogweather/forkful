---
title:                "Lavorare con i CSV"
aliases:
- it/typescript/working-with-csv.md
date:                  2024-02-03T19:21:33.942683-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

Lavorare con i file CSV (Valori Separati da Virgola) implica la lettura e la scrittura di file CSV, un formato comune di scambio dati utilizzato per la sua semplicità e ampia compatibilità attraverso varie piattaforme e linguaggi. I programmatori interagiscono con i file CSV per importare o esportare dati da applicazioni, database e servizi, permettendo una facile manipolazione e condivisione dei dati.

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
