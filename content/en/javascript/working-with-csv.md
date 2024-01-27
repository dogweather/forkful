---
title:                "Working with CSV"
date:                  2024-01-19
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma-Separated Values) in JavaScript usually means parsing and generating text data for spreadsheets or data transfer. Programmers do it because CSV is super common, lightweight, and easy to read or create.

## How to:

**Parsing CSV to JSON:**
```javascript
const csv = `name,age,city
Alice,30,New York
Bob,22,Los Angeles`;

function csvToJson(csv) {
  const lines = csv.split("\n");
  const headers = lines[0].split(",");
  return lines.slice(1).map(line => {
    const data = line.split(",");
    return headers.reduce((obj, nextKey, index) => {
      obj[nextKey] = data[index];
      return obj;
    }, {});
  });
}

console.log(csvToJson(csv));
// Output: [{name: 'Alice', age: '30', city: 'New York'}, {name: 'Bob', age: '22', city: 'Los Angeles'}]
```

**Generating CSV from JSON:**
```javascript
const jsonData = [
  { name: "Alice", age: 30, city: "New York" },
  { name: "Bob", age: 22, city: "Los Angeles" }
];

function jsonToCsv(json) {
  const headers = Object.keys(json[0]).join(",");
  const rows = json.map(obj =>
    Object.values(obj).join(",")
  ).join("\n");
  return `${headers}\n${rows}`;
}

console.log(jsonToCsv(jsonData));
// Output: name,age,city
//         Alice,30,New York
//         Bob,22,Los Angeles
```

## Deep Dive

CSV's been around since the early days of computing - easy for machines to process and humans to understand. But it's not perfect. If your data is complex or nested, JSON or XML might be a better fit. Implementation-wise, handling CSV in JavaScript needed workarounds due to its lack of a standard library for this; however, today numerous libraries like PapaParse or csv-parser simplify this task. Also, edge cases such as newline characters within fields and character encoding can complicate CSV handling and need careful coding attention.

## See Also

- MDN Web Docs on Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch (Grabbing CSV data from the web)
- PapaParse: https://www.papaparse.com/ (Robust CSV parser for the browser)
- RFC 4180: https://tools.ietf.org/html/rfc4180 (Standards for CSV files)
