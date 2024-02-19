---
aliases:
- /en/kotlin/working-with-csv/
date: 2024-02-03 19:03:21.344898-07:00
description: "Working with CSV (Comma-Separated Values) involves reading from and\
  \ writing data to CSV files, a common format for storing tabular data in plain text.\u2026"
lastmod: 2024-02-18 23:09:11.034960
model: gpt-4-0125-preview
summary: "Working with CSV (Comma-Separated Values) involves reading from and writing\
  \ data to CSV files, a common format for storing tabular data in plain text.\u2026"
title: Working with CSV
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) involves reading from and writing data to CSV files, a common format for storing tabular data in plain text. Programmers manipulate CSV files to easily exchange data between different applications, databases, or to facilitate data processing and analysis tasks.

## How to:

Kotlin, being a statically typed programming language that runs on the JVM, does not include a built-in library for handling CSV files. However, you can use the Java `BufferedReader` and `FileWriter` classes for basic operations, or leverage popular third-party libraries like `kotlinx.serialization` and `opencsv` for more advanced functionality.

### Reading a CSV file using BufferedReader:

```kotlin
import java.io.BufferedReader
import java.io.FileReader

fun main() {
    val path = "data.csv"
    val br = BufferedReader(FileReader(path))
    br.useLines { lines ->
        lines.forEach { line ->
            val cols = line.split(',')
            println(cols)
        }
    }
}
```

_Sample output:_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### Writing to a CSV file using FileWriter:

```kotlin
import java.io.FileWriter

fun main() {
    val data = listOf(
        listOf("Name", "Age", "City"),
        listOf("John Doe", "30", "New York"),
        listOf("Jane Smith", "25", "London")
    )

    FileWriter("output.csv").use { writer ->
        data.forEach { row ->
            writer.write(row.joinToString(",") + "\n")
        }
    }
}
```

This will create or overwrite `output.csv` with the provided data.

### Using kotlinx.serialization for CSV serialization:

First, add the dependency to your `build.gradle.kts`:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_Note: Ensure you have the correct version and repository configuration._

Then, define your data class and use `Csv` format for serialization:

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class Person(val name: String, val age: Int, val city: String)

fun main() {
    val csvFormat = Csv { delimiter = ',' }
    val data = listOf(
        Person("John Doe", 30, "New York"),
        Person("Jane Smith", 25, "London")
    )

    val csvData = csvFormat.encodeToString(data)
    println(csvData)
}
```

_Sample output:_

```
John Doe,30,New York
Jane Smith,25,London
```

### Using OpenCSV for advanced operations:

Add OpenCSV to your project's dependencies:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

Reading and writing with OpenCSV:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // Reading CSV
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // Writing CSV
    CSVWriter(FileWriter("output.csv")).use { csvWriter ->
        val entries = listOf(
            arrayOf("Name", "Age", "City"),
            arrayOf("John Doe", "30", "New York"),
            arrayOf("Jane Smith", "25", "London")
        )
        csvWriter.writeAll(entries)
    }
}
```

These code snippets demonstrate the flexibility Kotlin offers when working with CSV files, allowing you to choose the method that best fits your project needs.
