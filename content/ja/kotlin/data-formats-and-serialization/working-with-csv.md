---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:45.600557-07:00
description: null
lastmod: '2024-04-05T21:53:42.975038-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\u306F\u9759\u7684\u578B\u4ED8\u3051\u30D7\u30ED\u30B0\u30E9\u30DF\
  \u30F3\u30B0\u8A00\u8A9E\u3067\u3042\u308A\u3001JVM\u4E0A\u3067\u5B9F\u884C\u3055\
  \u308C\u307E\u3059\u304C\u3001CSV\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u305F\
  \u3081\u306E\u7D44\u307F\u8FBC\u307F\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u542B\u3093\
  \u3067\u3044\u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001\u57FA\u672C\u7684\u306A\
  \u64CD\u4F5C\u306E\u305F\u3081\u306BJava\u306E`BufferedReader`\u3068`FileWriter`\u30AF\
  \u30E9\u30B9\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002\u307E\u305F\u3001\u3088\u308A\u9AD8\u5EA6\u306A\u6A5F\u80FD\u306B\u306F\u3001\
  `kotlinx.serialization`\u3084`opencsv`\u306E\u3088\u3046\u306A\u4EBA\u6C17\u306E\
  \u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3092\u6D3B\u7528\u3067\u304D\u307E\u3059\u3002"
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 方法:
Kotlinは静的型付けプログラミング言語であり、JVM上で実行されますが、CSVファイルを扱うための組み込みライブラリを含んでいません。しかし、基本的な操作のためにJavaの`BufferedReader`と`FileWriter`クラスを使用することができます。また、より高度な機能には、`kotlinx.serialization`や`opencsv`のような人気のあるサードパーティライブラリを活用できます。

### BufferedReaderを使用したCSVファイルの読み取り:
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

_サンプル出力:_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### FileWriterを使用したCSVファイルへの書き込み:
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

これにより、提供されたデータで`output.csv`が作成または上書きされます。

### kotlinx.serializationを使用したCSVシリアライゼーション:
まず、`build.gradle.kts`に依存関係を追加します:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_注: 正しいバージョンとリポジトリの設定を確認してください。_

次に、データクラスを定義し、シリアライゼーションに`Csv`フォーマットを使用します:

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

_サンプル出力:_

```
John Doe,30,New York
Jane Smith,25,London
```

### OpenCSVを使用した高度な操作:
プロジェクトの依存関係にOpenCSVを追加します:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

OpenCSVでの読み取りと書き込み:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // CSVの読み取り
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // CSVの書き込み
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

これらのコードスニペットは、KotlinがCSVファイルを操作する際の柔軟性を示しており、プロジェクトのニーズに最も合った方法を選択できます。
