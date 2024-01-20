---
title:                "Working with csv"
html_title:           "C recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) involves reading and writing data in a text format where each line has fields separated by commas. Programmers use it because it's a simple, widely-supported way to exchange structured data between systems and applications.

## How to:

To work with CSV in Kotlin, you can use the core library or third-party libs like Kotlinx.serialization or Apache Commons CSV. Here, I'll show you the basic I/O without external libs.

```kotlin
import java.io.File

fun main() {
    // Writing to CSV
    val outputFile = File("data.csv")
    outputFile.printWriter().use { out ->
        out.println("id,name,age")
        out.println("1,John Doe,30")
        out.println("2,Jane Smith,25")
    }

    // Reading from CSV
    File("data.csv").forEachLine { line ->
        val (id, name, age) = line.split(',')
        println("ID: $id, Name: $name, Age: $age")
    }
}
```

Output:
```text
ID: 1, Name: John Doe, Age: 30
ID: 2, Name: Jane Smith, Age: 25
```

## Deep Dive

CSV's roots go back to the early days of computing when memory was limited and data interchange formats needed to be simple. While alternatives like JSON and XML have emerged, CSV remains popular for its ease of use, compatibility, and because it's human-readable.

Proper CSV handling can be more complex due to edge cases (like commas in data, multi-line fields, etc.). Libraries like Apache Commons CSV and Kotlinx.serialization handle these cases and provide additional functionality.

## See Also

- [RFC 4180](https://tools.ietf.org/html/rfc4180): The common format and MIME type for CSV files.
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/): A Java library for handling CSV files which can be used in Kotlin.
- [Kotlinx.serialization CSV](https://github.com/Kotlin/kotlinx.serialization): A Kotlin library that makes serialization to and from CSV format simpler.