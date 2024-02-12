---
title:                "处理CSV文件"
date:                  2024-02-03T19:20:49.286010-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理CSV文件"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

处理 CSV（逗号分隔值）涉及从 CSV 文件读取和写入数据，这是一种常见的以纯文本形式存储表格数据的格式。程序员操作 CSV 文件是为了方便地在不同应用程序、数据库之间交换数据，或者便于进行数据处理和分析任务。

## 如何操作：

Kotlin 是一种在 JVM 上运行的静态类型编程语言，并没有内置用于处理 CSV 文件的库。然而，你可以使用 Java 的 `BufferedReader` 和 `FileWriter` 类进行基本操作，或者利用流行的第三方库如 `kotlinx.serialization` 和 `opencsv` 来实现更高级的功能。

### 使用 BufferedReader 读取 CSV 文件：

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

_示例输出：_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### 使用 FileWriter 写入 CSV 文件：

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

这将创建或覆盖 `output.csv` 并用提供的数据填充。

### 使用 kotlinx.serialization 进行 CSV 序列化：

首先，在你的 `build.gradle.kts` 中添加依赖项：

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_注意：确保你有正确的版本和仓库配置。_

然后，定义你的数据类并使用 `Csv` 格式进行序列化：

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

_示例输出：_

```
John Doe,30,New York
Jane Smith,25,London
```

### 使用 OpenCSV 进行高级操作：

将 OpenCSV 添加到项目的依赖项中：

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

使用 OpenCSV 进行读写操作：

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // 读取 CSV
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // 写入 CSV
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

这些代码片段展示了 Kotlin 在处理 CSV 文件时提供的灵活性，允许你选择最适合你项目需求的方法。
