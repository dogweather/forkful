---
title:                "Elm recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/working-with-csv.md"
---

{{< edit_this_page >}}

# Why

CSV (Comma Separated Values) files are a popular way to store and exchange tabular data. They are easy to read and write, making them a common format for data storage and transfer. In this post, we'll explore how to work with CSV files in Elm.

# How To

To work with CSV files in Elm, we first need to import the necessary modules:

```
import Csv
import File
```

Next, we can use the `File.toCsvString` function to convert a list of lists into a CSV string:

```
File.toCsvString [[1,2,3], [4,5,6], ["a", "b", "c"]]
```

Output:

```
1,2,3
4,5,6
a,b,c
```

We can also use the `Csv.fromString` function to parse a CSV string into a list of lists:

```
Csv.fromString "1,2,3\n4,5,6\na,b,c"
```

Output:

```
[[1,2,3], [4,5,6], ["a", "b", "c"]]
```

If our CSV file has header columns, we can use the `withColumnNames` function to include them in the output:

```
Csv.fromString "Name,Age,Occupation\nJohn,25,Engineer\nSarah,30,Teacher"
    |> Csv.withColumnNames
```

Output:

```
[["Name","Age","Occupation"], ["John", "25", "Engineer"], ["Sarah", "30", "Teacher"]]
```

We can also use the `File.readCsv` function to read a CSV file from our local file system and convert it into a list of lists:

```
File.readCsv "data.csv"
```

Output:

```
[[1,2,3], [4,5,6], ["a", "b", "c"]]
```

# Deep Dive

Working with CSV files in Elm can get more complicated when dealing with different delimiters, quote characters, and new line characters. To handle these variations, the `Csv` module provides options for custom delimiter and quote characters, as well as the ability to handle different new line formats.

In addition, the `Csv.Decode` module allows us to decode CSV files into Elm types, providing better type safety and error handling. This module also includes functions for converting CSV values into lists, records, or custom types.

# See Also

For more information on working with CSV files in Elm, check out the official documentation and this tutorial on handling CSV data with Elm.