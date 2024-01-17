---
title:                "Lavorare con i file csv."
html_title:           "TypeScript: Lavorare con i file csv."
simple_title:         "Lavorare con i file csv."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con CSV significa manipolare file di testo contenenti dati separati da una virgola. I programmatori spesso lavorano con CSV quando vogliono importare o esportare dati da un'applicazione o database.

## Come si fa:
Ecco un semplice esempio di come si può leggere e scrivere un file CSV in TypeScript.

```TypeScript
import * as fs from 'fs';
import * as csv from 'csv-parser';

// Leggere un file CSV
fs.createReadStream('dati.csv')
  .pipe(csv())
  .on('data', (row) => {
    console.log(row);
  })
  .on('end', () => {
    console.log('Lettura del file CSV completata');
  });

// Scrivere un file CSV
let dati = [
  { nome: 'Mario', cognome: 'Rossi', eta: 35 },
  { nome: 'Giulia', cognome: 'Bianchi', eta: 27 },
  { nome: 'Luca', cognome: 'Verdi', eta: 40 }
];

const stringify = require('csv-stringify');
let csvString = stringify(dati, { header: true });

fs.writeFile('nuovi_dati.csv', csvString, (err) => {
  if (err) throw err;
  console.log('Scrittura del file CSV completata');
});
```

L'output di questo codice sarà una visualizzazione dei dati del CSV in formato oggetto e la creazione di un nuovo file CSV con i dati forniti.

## Approfondimento:
Il formato CSV è stato inventato negli anni '70 per fornire uno standard comune per lo scambio di dati tra diversi sistemi informatici. Alcune alternative al formato CSV sono il formato JSON e il formato XML, che offrono una maggiore struttura e flessibilità dei dati. TypeScript fornisce il package 'csv-parser' per il parsing dei file CSV e il package 'csv-stringify' per la creazione di nuovi file CSV.

## Vedi anche:
- Documentazione package csv-parser: https://www.npmjs.com/package/csv-parser
- Documentazione package csv-stringify: https://www.npmjs.com/package/csv-stringify