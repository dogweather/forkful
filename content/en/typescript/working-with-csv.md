---
title:                "TypeScript recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma Separated Values) files are a common format for storing and exchanging data. Many applications, such as Microsoft Excel, use CSV files for importing and exporting large datasets. Therefore, being able to work with CSV files is a valuable and essential skill for anyone working with data.

## How To

To work with CSV files in TypeScript, we can use the built-in `fs` module which provides methods for reading and writing files. First, we need to import the module in our TypeScript file:

```TypeScript
import fs from 'fs';
```

Next, we need to read the CSV file and parse its contents into an array of objects. In this example, we will use the `readFileSync` method to read the file synchronously:

```TypeScript
const csvFile = fs.readFileSync('./sample.csv', 'utf8');
const rows = csvFile.split('\n');
const headers = rows[0].split(',');
const data = rows.slice(1).map((row) => {
  const values = row.split(',');

  return values.reduce(
    (obj: { [key: string]: string }, val: string, i: number) => {
      const header = headers[i].trim();
      return { ...obj, [header]: val.trim() };
    },
    {}
  );
});
```

The code above reads the CSV file, splits it into rows, and then splits each row into an array of values. This array of values is then mapped to an object with keys based on the headers from the first row of the CSV file. This way, we can access the data in a more structured format.

To write data to a CSV file, we can use the `writeFileSync` method and pass in the data in a properly formatted string:

```TypeScript
const dataToWrite = `Name, Age, Favorite Food\n
John, 25, Pizza\n
Sarah, 30, Sushi\n`;
fs.writeFileSync('./output.csv', dataToWrite);
```

The above code will create a new CSV file with the specified data.

## Deep Dive

When working with CSV files, it is important to take into account any potential issues with the data, such as missing or invalid values. One common problem is when a CSV file contains a comma within one of its values, which can throw off the mapping process. To handle this, we can use a CSV parsing library like `csv-parser` which allows us to specify a custom delimiter, making it easier to handle special cases.

Another consideration is how to handle large CSV files. In some cases, the entire file may not fit into memory, which can cause performance issues. To avoid this, we can use `readline`, a built-in module that allows us to read a file line by line, rather than reading the whole file at once.

## See Also

- [Node.js fs module documentation](https://nodejs.org/api/fs.html)
- [csv-parser library](https://www.npmjs.com/package/csv-parser)
- [Node.js readline module documentation](https://nodejs.org/api/readline.html)