---
title:                "Lavorare con i file CSV"
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con CSV significa manipolare dati salvati come valore separati da virgole, un formato popolare per l'importazione ed esportazione di dati in applicazioni. Programmer usano CSV per la sua semplicità e intercompatibilità con diversi sistemi e linguaggi di programmazione.

## How to:
Installare `csv-parser`, una libreria popolare, tramite npm:

```bash
npm install csv-parser
```

Esempio di lettura di un CSV in TypeScript:

```typescript
import * as fs from 'fs';
import * as csv from 'csv-parser';

const results: any[] = [];

fs.createReadStream('dati.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
  });
```
Dopo aver eseguito lo script, l'output sarà un array di oggetti contenenti i dati del CSV.

## Deep Dive
CSV è un formato standardizzato negli anni '70, ed è diventato essenziale per lo scambio di informazioni tabellari. Alternative includono JSON e XML, ma CSV rimane il re per la sua leggibilità e leggerezza. Implementando la lettura CSV in TypeScript, si deve gestire la tipizzazione e possibili errori di parsing.

## See Also
- Documentazione ufficiale di `csv-parser`: [npm csv-parser](https://www.npmjs.com/package/csv-parser)
- Guida TypeScript ufficiale: [TypeScript Documentation](https://www.typescriptlang.org/docs/)