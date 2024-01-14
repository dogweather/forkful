---
title:                "TypeScript: Lavorare con file csv."
simple_title:         "Lavorare con file csv."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molti formati di file diversi che gli sviluppatori possono incontrare durante il loro lavoro, ma uno dei più comuni è il formato CSV. Questo file è comunemente utilizzato per archiviare dati tabellari, come fogli di calcolo, e può essere facilmente letto e scritto utilizzando TypeScript.

## Come fare
Per prima cosa, è necessario installare il pacchetto csv-parser utilizzando npm. Una volta fatto ciò, possiamo utilizzare il seguente codice TypeScript per leggere un file CSV e stamparne i contenuti:

```TypeScript
import * as fs from 'fs';
import * as csvParser from 'csv-parser';

fs.createReadStream('data.csv')
  .pipe(csvParser())
  .on('data', (data) => console.log(data))
  .on('end', () => console.log('Lettura del file CSV completata.'));
```

Se il file CSV contiene i seguenti dati:

```csv
nome,cognome,età
Marco,Rossi,25
Giulia,Bianchi,30
```

L'output del codice precedente sarà:

```
{ nome: 'Marco', cognome: 'Rossi', eta: '25' }
{ nome: 'Giulia', cognome: 'Bianchi', eta: '30' }
```

Possiamo anche scrivere su un file CSV utilizzando il seguente codice TypeScript:

```TypeScript
import * as fs from 'fs';
import * as csvWriter from 'csv-writer';

const dati = [
  { nome: 'Francesca', cognome: 'Verdi', eta: '28' },
  { nome: 'Luca', cognome: 'Neri', eta: '32' },
];

const csvWriter = createObjectCsvWriter({
  path: 'output.csv',
  header: ['nome', 'cognome', 'età']
});

csvWriter.writeRecords(dati)
  .then(() => console.log('Scrittura su file CSV completata.'));
```

Questo codice scriverà i dati nell'array "dati" su un file CSV chiamato "output.csv" con le intestazioni delle colonne "nome", "cognome" e "età". Il contenuto di "output.csv" sarà il seguente:

```
nome,cognome,età
Francesca,Verdi,28
Luca,Neri,32
```

## Approfondimento
Ci sono molte altre librerie e pacchetti che possono essere utilizzati con TypeScript per lavorare con file CSV, come ad esempio csv-parser, csv-writer e Papa Parse. Inoltre, è possibile leggere e scrivere dati CSV da e verso database utilizzando librerie come knex.js e TypeORM.

## Vedi anche
- npm: https://www.npmjs.com/
- csv-parser: https://www.npmjs.com/package/csv-parser
- csv-writer: https://www.npmjs.com/package/csv-writer
- Papa Parse: https://www.npmjs.com/package/papaparse
- knex.js: https://knexjs.org/
- TypeORM: https://typeorm.io/#/