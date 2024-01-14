---
title:                "Kotlin recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma Separated Values) is a commonly used file format for storing tabular data. It is widely supported by various software and applications, making it a popular choice for data exchange. In this blog post, we will explore how to work with CSV files in Kotlin, one of the fastest-growing programming languages.

## How To

To start working with CSV in Kotlin, we first need to add the OpenCSV library to our project. This library provides an easy-to-use API for reading and writing CSV files. To add the library to our project, we can use Gradle by adding the following dependency in our `build.gradle` file:

```Kotlin
implementation 'com.opencsv:opencsv:5.5.1'
```

Once the library is added, we can use it to read and write CSV files in our code. Let's take a look at an example:

```Kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter

// initialize the CSVReader and CSVWriter objects
val reader = CSVReader("input.csv")
val writer = CSVWriter("output.csv")

// read data from the CSV file and store it in a list
val data = reader.readAll()

// modify the data as needed
data.forEach { record ->
    record[2] = "New Value" // for example, changing the third column of each row
}

// write the modified data into a new CSV file
data.forEach { record ->
    writer.writeNext(record.toTypedArray())
}

// close the reader and writer objects
reader.close()
writer.close()
```

In the above code, we used the `CSVReader` to read data from a CSV file and store it in a list. Then, we used the `CSVWriter` to write the modified data into a new CSV file. This is just a simple example to demonstrate how to work with CSV files in Kotlin using the OpenCSV library. There are many more methods and configurations available in the library, which you can explore in the official documentation.

## Deep Dive

While CSV files may seem simple, there are some nuances to keep in mind when working with them. For example, CSV files can have headers, where the first row contains the names of the columns. When reading a CSV file with headers, we can use the `CSVReader#readNext()` method to skip the first row and not include it in our data list. Additionally, we can also specify the delimiter and quote character used in a CSV file. These are just a few examples of the configurations available in the OpenCSV library.

Another important aspect to keep in mind when working with CSV files is data parsing and validation. As CSV files are plain text files, there is no built-in data type for each column. Hence, it is crucial to properly parse and validate the data according to our needs. For example, we might need to convert a string value to an integer or a date object. We can use libraries like `kotlinx-datetime` or `SimpleDateFormat` to handle these conversions.

## See Also

- [OpenCSV Official Documentation](http://opencsv.sourceforge.net/)
- [Kotlinx-datetime Documentation](https://github.com/Kotlin/kotlinx-datetime)
- [SimpleDateFormat Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)