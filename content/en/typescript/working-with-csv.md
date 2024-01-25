---
title:                "Working with CSV"
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) means reading and writing data in a text format where each line is a data record, and commas separate each field. Programmers use CSV for its simplicity and wide support across tools for data exchange.

## How to:

Reading CSV in TypeScript is straightforward with libraries like `papaparse`. To handle CSV files, install it first:

```bash
npm install papaparse
```

Here's how you read a CSV file:

```typescript
import * as fs from 'fs';
import * as Papa from 'papaparse';

const csvFilePath = 'path/to/your/file.csv';
const fileContent = fs.readFileSync(csvFilePath, 'utf8');

Papa.parse(fileContent, {
  complete: (result) => {
    console.log(result.data);
  }
});
```

To write CSV, you might use `csv-writer`. Install it with:

```bash
npm install csv-writer
```

And then write to a CSV file like so:

```typescript
import * as createCsvWriter from 'csv-writer';

const csvWriter = createCsvWriter.createObjectCsvWriter({
  path: 'path/to/your/output.csv',
  header: [
    {id: 'name', title: 'NAME'},
    {id: 'age', title: 'AGE'}
  ]
});

const data = [
  { name: 'John', age: 28 },
  { name: 'Jane', age: 32 }
];

csvWriter.writeRecords(data)
  .then(() => console.log('Data written to CSV file successfully.'));
```

The output in 'output.csv' will be:

```
NAME,AGE
John,28
Jane,32
```

## Deep Dive

CSV has been a staple in data exchange since the early computer era due to its readability and simplicity. It's not without issues; for instance, lack of standardization can lead to parsing errors. Alternatives like JSON and XML offer more complex structures and data types. When implementing CSV parsers/writers, consider character encoding and correct handling of special characters to avoid bugs.

## See Also

- The `papaparse` documentation: [Papa Parse - Powerful CSV Parser](https://www.papaparse.com/)
- The `csv-writer` documentation: [CSV Writer - CSV File Writer for Node](https://csv.js.org/)
- For deeper technical understanding, the RFC 4180 document provides the de facto standard for CSV formats: [RFC 4180](https://tools.ietf.org/html/rfc4180)
- For a comparison of file formats, see: [JSON vs XML vs CSV](https://www.geeksforgeeks.org/difference-between-json-and-xml/)