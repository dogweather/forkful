---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:16.552590-07:00
description: "Lavorare con CSV (Valori Separati da Virgola) in JavaScript comporta\
  \ l'analisi o la generazione di file CSV per ingerire dati tabellari da fonti esterne\u2026"
lastmod: '2024-03-13T22:44:43.836023-06:00'
model: gpt-4-0125-preview
summary: Lavorare con CSV (Valori Separati da Virgola) in JavaScript comporta l'analisi
  o la generazione di file CSV per ingerire dati tabellari da fonti esterne oppure
  esportare dati per l'utilizzo in altri programmi.
title: Lavorare con i CSV
weight: 37
---

## Come fare:
JavaScript non ha funzionalità incorporate per l'analisi o la creazione di stringhe CSV come ha con JSON. Tuttavia, è possibile gestire facilmente i dati CSV utilizzando JavaScript puro per compiti più semplici o sfruttando potenti librerie come `PapaParse` per scenari più complessi.

### Analisi di Base con JavaScript Puro
Per analizzare una semplice stringa CSV in un array di oggetti:

```javascript
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const lines = csv.split("\n");
  const result = [];
  const headers = lines[0].split(",");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split(",");
    
    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }
    result.push(obj);
  }
  
  return result;
}

console.log(parseCSV(csv));
```
Output:

```
[
  { name: 'John', age: '23', city: 'New York' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### Generazione di Base in CSV con JavaScript Puro
Per convertire un array di oggetti in una stringa CSV:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(row => 
    Object.values(row).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

Output:

```
John,23,New York
Jane,28,Los Angeles
```

### Utilizzo di PapaParse per Compiti CSV Complessi
Per scenari più complessi, `PapaParse` è una libreria robusta adatta per l'analisi e la creazione di stringhe CSV con opzioni per stream, worker e gestione di file di grandi dimensioni.

Analisi di file o stringa CSV con PapaParse:

```javascript
// Dopo aver aggiunto PapaParse al tuo progetto
const Papa = require('papaparse');
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Analizzato:", results.data);
  }
});
```

Genera:

```
Analizzato: [
  ["name", "age", "city"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

Creazione di una stringa CSV da un array con PapaParse:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

Genera:

```
name,age,city
John,23,New York
Jane,28,Los Angeles
```

Questi esempi illustrano la gestione dei CSV in JavaScript a livello basico e avanzato, permettendo uno scambio di dati semplice in applicazioni web e oltre.
