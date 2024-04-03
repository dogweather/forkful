---
date: 2024-02-03 19:03:07.378496-07:00
description: "How to: In TypeScript, you can work with CSV files through native code\
  \ or by leveraging third-party libraries like `csv-parser` for reading and `csv-\u2026"
lastmod: '2024-03-13T22:44:59.878411-06:00'
model: gpt-4-0125-preview
summary: In TypeScript, you can work with CSV files through native code or by leveraging
  third-party libraries like `csv-parser` for reading and `csv-writer` for writing
  CSV files.
title: Working with CSV
weight: 37
---

## How to:
In TypeScript, you can work with CSV files through native code or by leveraging third-party libraries like `csv-parser` for reading and `csv-writer` for writing CSV files.

### Reading CSV with `csv-parser`
First, install `csv-parser` via npm:

```
npm install csv-parser
```

Then, read a CSV file like so:

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // Output: Array of objects, each representing a row in the CSV
  });
```

Assuming `data.csv` contains:

```
name,age
Alice,30
Bob,25
```

The output will be:

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### Writing CSV with `csv-writer`
To write to a CSV file, first install `csv-writer`:

```
npm install csv-writer
```

Then, use it as follows:

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
  .then(() => console.log('The CSV file was written successfully'));
```

This code writes the following to `out.csv`:

```
NAME,AGE
Alice,30
Bob,25
```

These examples show how to integrate CSV processing in your TypeScript projects efficiently, whether it's reading data for analysis or persisting application data externally.
