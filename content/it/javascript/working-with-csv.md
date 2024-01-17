---
title:                "Lavorare con i file csv"
html_title:           "Javascript: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con CSV significa lavorare con il formato di file "Comma Separated Values" (Valori delimitati da virgole), che viene comunemente utilizzato per archiviare dati in forma tabellare. I programmatori lo fanno per importare o esportare dati da un sistema a un altro, o per manipolare facilmente grandi quantità di dati.

## Come fare:
Utilizzare la libreria "csv-parser" per analizzare un file CSV e accedere ai dati in esso contenuti:
```Javascript
const csv = require('csv-parser');
fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (row) => {
    console.log(row);
  })
  .on('end', () => {
    console.log('CSV file successfully processed');
  });
```

Utilizzare la libreria "csv-writer" per creare un nuovo file CSV e scrivere i dati al suo interno:
```Javascript
const createCsvWriter = require('csv-writer').createObjectCsvWriter;
const csvWriter = createCsvWriter({
    path: 'output.csv',
    header: [
        {id: 'name', title: 'Nome'},
        {id: 'age', title: 'Età'},
    ]
});
const data = [
    {
        name: 'Mario',
        age: 30,
    },
    {
        name: 'Luigi',
        age: 35,
    }
];
csvWriter
    .writeRecords(data)
    .then(() => console.log('File CSV creato con successo'));
```

Utilizzare la funzione nativa "JSON.stringify" per convertire un oggetto in formato CSV:
```Javascript
const data = [
    {
        name: 'Pippo',
        age: 25,
    },
    {
        name: 'Topolino',
        age: 40,
    }
];
const csv = JSON.stringify(data, ['name', 'age']);
console.log(csv);
```

## Approfondimenti:
Il formato del file CSV è stato creato nel 1972 e ha subito diverse evoluzioni nel tempo. Oggi è uno dei formati più diffusi per l'interscambio di dati.

Un'alternativa all'utilizzo di un file CSV è l'utilizzo di un database relazionale, che permette di organizzare i dati in tabelle e di eseguire query complesse.

Per lavorare con i CSV in modo più efficiente, si può utilizzare la funzionalità di stream di Node.js, che permette di leggere e scrivere grandi quantità di dati in modo incrementale.

## Vedi anche:
- [Libreria "csv-parser"](https://www.npmjs.com/package/csv-parser)
- [Libreria "csv-writer"](https://www.npmjs.com/package/csv-writer)
- [Articolo di riferimento su come lavorare con CSV in Node.js](https://stackabuse.com/reading-and-writing-csv-files-with-node-js/)