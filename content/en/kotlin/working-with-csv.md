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

## Why

Are you tired of manually organizing and managing large datasets? Look no further, because CSV (Comma Separated Values) is here to save your day! With CSV, you can easily store and retrieve data in a simple and versatile format. In this article, we will explore how to work with CSV in Kotlin and make your data management process much more efficient.

## How To

To begin working with CSV in Kotlin, we need to add the OpenCSV dependency in our project:

```
implementation("com.opencsv:opencsv:5.3")
```

Next, we need to import the necessary classes into our project:

```
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
```

### Reading CSV

To read data from a CSV file, we will use the `CSVReader` class. This class takes in a `FileReader` object as a parameter, representing the CSV file we want to read from. We can then use the `readAll()` method to retrieve all the data from the CSV file.

```
// Create a FileReader object with our CSV file
val fileReader = FileReader("data.csv")
// Create a CSVReader object with the fileReader object
val csvReader = CSVReader(fileReader)
// Use the readAll() method to get all the data from the CSV file
val data = csvReader.readAll()
```

The `data` variable will now contain a list of lists, with each inner list representing a row of data from the CSV file. We can then iterate through this list to access and manipulate our data as needed.

### Writing CSV

To write data to a CSV file, we will use the `CSVWriter` class. Similar to the `CSVReader` class, we will need to provide a `FileWriter` object as a parameter. We can then use the `writeAll()` method to write a list of lists to the CSV file.

```
// Create a FileWriter object with the CSV file we want to write to
val fileWriter = FileWriter("data.csv")
// Create a CSVWriter object with the fileWriter object
val csvWriter = CSVWriter(fileWriter)
// Create a list of data we want to write to the CSV file
val newData = listOf(listOf("John", "Smith", "35"), listOf("Mary", "Johnson", "28"))
// Use the writeAll() method to write the data to the CSV file
csvWriter.writeAll(newData)
```

This will write the data from the `newData` list to the CSV file in the format of "John, Smith, 35" and "Mary, Johnson, 28" on separate rows.

## Deep Dive

CSV files are a popular choice for storing and transferring data because they are lightweight, easy to read and write, and can be opened with any text editor. Additionally, CSV files are compatible with different software and programming languages, making it a versatile choice for data management.

Although CSV is a simple format, it can also be customized to fit specific data needs. For example, we can choose to use different delimiters (such as a tab or semicolon) instead of a comma, or add headers to our CSV file to label our data.

It's also important to note that CSV files may not be suitable for complex data structures or data that requires strict data types. In these cases, it may be better to use other file formats such as JSON or XML.

## See Also

- [Official Kotlin Website](https://kotlinlang.org/)
- [OpenCSV Documentation](http://opencsv.sourceforge.net/)
- [Working with CSV files in Java](https://www.baeldung.com/java-csv-file-array)