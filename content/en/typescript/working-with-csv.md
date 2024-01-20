---
title:                "Working with csv"
html_title:           "TypeScript recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma Separated Values) involves writing and reading data from a plain text file with values separated by a comma. Programmers use CSV as it is a simple and efficient way to store and transfer structured data.

## How to:
```TypeScript
//Reading CSV data into an array
import * as fs from 'fs';

const csvData = fs.readFileSync('data.csv', 'utf-8').split('\n').map(row => row.split(','))

//Writing data to a CSV file
csvData.forEach(row => fs.appendFileSync('newData.csv', row.join(',') + '\n'))

//Sample CSV data
Name, Age, Occupation
John, 25, Developer
Jane, 30, Designer
```

## Deep Dive:
CSV was first introduced in the 1970s and gained popularity as a way to store large amounts of data in a simple and readable format. It is widely used in data analysis, database management, and data transfer between different systems. Alternatives to CSV include XML and JSON, but CSV is more human-readable and lightweight.

Although CSV is a simple format, programmers should consider edge cases such as handling quotes and special characters, determining the correct encoding, and dealing with uneven columns in the data.

## See Also:
- [Simple CSV node package](https://www.npmjs.com/package/simple-csv)
- [CSV vs XML vs JSON](https://softwareengineering.stackexchange.com/questions/123947/comparison-of-data-interchange-formats)