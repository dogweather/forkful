---
title:                "Working with csv"
html_title:           "Kotlin recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma Separated Values) involves manipulating data in a tabular form, with each line representing a record and each value separated by a comma. Programmers use CSV to store and analyze large amounts of data efficiently.

## How to:

Working with CSV files in Kotlin is made easy with the help of external libraries such as OpenCSV and Kotlin-CSV. These libraries provide convenient methods and classes to handle CSV files.

To read a CSV file, we can use the `Reader` class from the OpenCSV library. First, we need to create a `Reader` object, passing in the path of the CSV file as a parameter. We can then use the `readAll()` method to read all the records from the file and store them in a list.

```Kotlin
val reader = CSVReader(FileReader("path/to/file.csv"))
val records = reader.readAll()
```

To write data to a CSV file, we can use the `CsvWriter` class from the Kotlin-CSV library. First, we need to create a `CsvWriter` object, passing in the path of the CSV file and the header names as parameters. We can then write each record using the `writeRow()` method.

```Kotlin
val writer = CsvWriter("path/to/file.csv", *arrayOf("Name", "Age", "Email"))
writer.writeRow("John", "25", "john@example.com")
writer.writeRow("Jane", "30", "jane@example.com")
```

## Deep Dive:

CSV has been in use since the 1970s and is still widely used for data exchange between different applications. It is a simple and lightweight format, making it easy to work with large datasets. However, manipulating data in CSV format can be tricky, especially when dealing with special characters or empty values.

As an alternative to CSV, some programmers prefer using JSON or XML for storing and exchanging data. These formats provide more structured data and have built-in support in Kotlin, making them easier to work with.

When working with CSV files, it's essential to consider the encoding of the file, as it can affect the accuracy of the data. By default, most CSV libraries use UTF-8 encoding, which may not be suitable for all datasets.

## See Also:

- OpenCSV library: https://opencsv.sourceforge.net/
- Kotlin-CSV library: https://github.com/doyaaaaaken/kotlin-csv
- Kotlin for Data Science: https://kotlinlang.org/docs/tutorials/data-science-overview.html