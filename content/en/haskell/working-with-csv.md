---
title:                "Haskell recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma-Separated Values) files are a common way to store and share data in a structured format. Many software programs, such as Microsoft Excel, use CSV as their default file type for exporting data. Therefore, as a programmer, knowing how to work with CSV files can greatly benefit you in terms of data manipulation and analysis.

## How To

Working with CSV files in Haskell is made easy thanks to the `Data.Csv` library. First, we need to import the necessary modules:

```Haskell
import Data.Csv
import qualified Data.ByteString.Lazy as BL
```

Next, we can read a CSV file using the `decode` function and passing in the file contents as a `ByteString`. The `decode` function returns a `Either String (Vector (Vector ByteString))` which represents either an error message or a vector of vectors containing the data from the CSV file.

```Haskell
csvData <- BL.readFile "file.csv"
decodedData <- return (decode NoHeader csvData) :: IO (Either String (Vector (Vector ByteString)))
```

We can then access specific rows and columns of the `decodedData` using the indexing operator `!!`. For example, to access the second row and first column, we can do:

```Haskell
singleValue <- return (decodedData !! 1 !! 0) :: IO (ByteString)
```

To convert the `ByteString` values to more usable types, we can use the `parseField` function from `Data.Csv` and specify the desired type.

```Haskell
convertedValue <- return (parseField singleValue :: Either String Int)
```

Lastly, we can write data to a CSV file using the `encode` function. For example, to create a new CSV file with a single row containing the converted value from earlier, we can do:

```Haskell
let newRow = [convertedValue]
BL.writeFile "newFile.csv" (encode [newRow])
```

## Deep Dive

The `Data.Csv` library provides many functions for working with CSV files, such as handling headers, custom delimiters, and automatically converting data to different types. It also uses a row-oriented parser which allows for efficient processing of large CSV files. Additionally, the library has good error handling for when the CSV file does not adhere to the expected format.

When working with CSV files, it's important to understand the structure of the data and the intended use of the file. This will help in determining the appropriate functions to use and how to handle any discrepancies or errors in the data.

## See Also

- [Data.Csv library documentation](https://hackage.haskell.org/package/cassava)
- [Haskell CSV tutorial](https://bartoszmilewski.com/2018/04/10/parsing-csv-files-in-haskell/)