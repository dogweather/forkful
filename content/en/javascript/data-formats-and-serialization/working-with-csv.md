---
title:                "Working with CSV"
aliases: - /en/javascript/working-with-csv.md
date:                  2024-02-03T19:03:29.199464-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma-Separated Values) in JavaScript entails parsing or generating CSV files to either ingest tabular data from external sources or export data for use in other programs. Programmers do this because it enables easy, lightweight data interchange between applications, databases, and systems where more complex formats like JSON might be overkill.

## How to:
JavaScript does not have built-in CSV parsing or stringifying functionality like it does with JSON. However, you can easily manage CSV data by using either raw JavaScript for simpler tasks or leveraging powerful libraries like `PapaParse` for more complex scenarios.

### Basic Parsing with Raw JavaScript
To parse a simple CSV string into an array of objects:

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

### Basic Generation to CSV with Raw JavaScript
To convert an array of objects into a CSV string:

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

### Using PapaParse for Complex CSV Tasks
For more complex scenarios, `PapaParse` is a robust library suitable for parsing and stringifying CSV files with options for streams, workers, and handling huge files.

Parsing CSV file or string with PapaParse:

```javascript
// After adding PapaParse to your project
const Papa = require('papaparse');
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Parsed:", results.data);
  }
});
```

Generates:

```
Parsed: [
  ["name", "age", "city"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

Stringifying an array to a CSV string with PapaParse:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

Generate:

```
name,age,city
John,23,New York
Jane,28,Los Angeles
```

These examples illustrate basic and advanced CSV handling in JavaScript, enabling easy data exchange in web applications and beyond.
