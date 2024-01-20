---
title:                "Working with csv"
html_title:           "Javascript recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma Separated Values) is a common task for programmers. It involves parsing and manipulating data stored in a text file with a specific format, where each value is separated by a comma. Programmers use CSV to import and export data from different sources, such as databases, spreadsheets, and other applications.

## How to:

To work with CSV in Javascript, there are a few useful methods and libraries available. The following examples will demonstrate how to read and write CSV files using the built-in `fs` module and the popular `csv-parser` library.

### Reading CSV files

To read a CSV file in Javascript, we can use the `readFile` method from the `fs` module. This method takes in the path of the file to be read and a callback function to handle the data.

```javascript
const fs = require('fs');

fs.readFile('data.csv', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

In the above example, the `data.csv` file is read and logged to the console. However, the output is still in a string format, and we need to convert it to an array of objects to work with the data effectively.

To do so, we can use the `csv-parser` library, which will automatically parse the CSV data and return an array of objects, with each object representing a row in the CSV file.

```javascript
const csv = require('csv-parser');
const fs = require('fs');

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (row) => {
    console.log(row);
  })
  .on('end', () => {
    console.log('CSV file successfully processed');
  });
```

The above code uses the `createReadStream` method from `fs` to create a readable stream from the CSV file. Then, the `pipe` method is used to pipe the stream to the `csv` parser. Finally, we can handle the data using the `on` method, where the `data` event is emitted for each row in the CSV file, and the `end` event is emitted when all the data has been processed.

### Writing CSV files

To write data to a CSV file, we can use the `writeFile` method from the `fs` module. This method takes in the path of the file to be written and the data to be written. However, since CSV requires each row to be in a string format, we need to convert our data to a string before writing it.

```javascript
const fs = require('fs');

const data = [
  { name: 'John', age: 25 },
  { name: 'Jane', age: 30 },
  { name: 'Bob', age: 35 },
];

const csv = data.map((row) => Object.values(row).join(','));

fs.writeFile('data.csv', csv.join('\n'), (err) => {
  if (err) throw err;
  console.log('CSV file successfully written');
});
```

In the above example, we have an array of objects representing the data we want to write to the CSV file. The `map` method is used to convert each object to a string, and the `join` method is used to join each value with a comma. Then, we use the `writeFile` method to write the CSV data to the file.

## Deep Dive

CSV has been in use since the early days of spreadsheets and databases, and it remains a popular format for exchanging tabular data. It is a simple and human-readable format that is supported by most programming languages. However, it does have some limitations, such as not being able to handle complex data types or nested structures. In such cases, other formats like JSON or XML may be more suitable.

Apart from the `csv-parser` library, there are other alternatives for working with CSV in Javascript, such as `fast-csv` and `csvtojson`. These libraries provide additional features and performance optimizations, which can be useful for larger CSV files. It is always recommended to research and compare different options before choosing the best one for your project.

When working with CSV, it is essential to have a good understanding of how the data is structured, as well as how to properly handle and format it. Handling uppercase vs lowercase letters, white spaces, and special characters can be challenging, so it is crucial to use well-tested libraries or consider using a CSV validation tool.

## See Also

- [RFC 4180 - Common Format and MIME Type for Comma-Separated Values (CSV) Files](https://tools.ietf.org/html/rfc4180)