---
title:                "Working with csv"
html_title:           "Haskell recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma Separated Values) files are commonly used in data analysis and management as they allow for easy storage and manipulation of data. Using Haskell to work with CSV files can provide efficient and effective solutions for handling large datasets. Additionally, learning how to work with CSV files in Haskell can enhance your programming skills and make you a more versatile developer.

## How To

To start working with CSV files in Haskell, we first need to import the necessary libraries:

```Haskell
import Text.CSV
import System.IO
```

Next, we can read a CSV file and store its contents in a variable:

```Haskell
exampleCSV <- parseCSVFromFile "example.csv"
```

We can then use the `parseCSVFromFile` function to parse the file and return a `Result` type. This type can either be `CSVError` if there was an error in parsing, or `CSV` if the file was successfully parsed.

To access the values from the CSV file, we can use the `rows` function to get a list of `Record` types:

```Haskell
let records = rows exampleCSV
```

We can then use the `!!` operator to access specific columns or rows in the CSV file. For example, to get the value in the first column of the second row:

```Haskell
let firstColSecondRow = records !! 1 !! 0
```

Alternatively, we can use the `record` function to access a specific row and column by name. For example, to get the value in the `Name` column of the second row:

```Haskell
let name = record records !! 1 ! "Name"
```

To write to a CSV file, we can use the `writeFile` function and pass in a `Record` type. For example, to write a new row to a CSV file:

```Haskell
let newRow = ["John", "35", "New York"]
writeFile "example.csv" $ recordToCSV newRow
```

## Deep Dive

Working with CSV files involves parsing the data, manipulating it, and then writing the changes back to the file. The `CSV` type returned by the `parseCSVFromFile` function is a list of `Record` types, which in turn are list of `Field` types. This allows us to easily access and modify specific values within the CSV file.

The `recordToCSV` function converts a `Record` type to a string in CSV format, which can then be written to a file. Additionally, we can use the `encode` function from the `Data.List.Split` library to manipulate the data in CSV format and then convert it back to a `Record` type.

## See Also

- [Haskell CSV library documentation](https://hackage.haskell.org/package/csv)
- [Haskell tutorials and resources](https://www.haskell.org/documentation)
- [Working with CSV files in Python](https://realpython.com/python-csv/)