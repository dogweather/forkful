---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:45.600557-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.095572-06:00'
model: gpt-4-0125-preview
summary: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u306E\u64CD\u4F5C\
  \u3068\u306F\u3001\u30D7\u30EC\u30FC\u30F3\u30C6\u30AD\u30B9\u30C8\u3067\u8868\u5F62\
  \u5F0F\u306E\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\u3059\u308B\u4E00\u822C\u7684\u306A\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3042\u308BCSV\u30D5\u30A1\u30A4\u30EB\
  \u304B\u3089\u306E\u8AAD\u307F\u53D6\u308A\u3068\u3001\u305D\u308C\u306B\u30C7\u30FC\
  \u30BF\u3092\u66F8\u304D\u8FBC\u3080\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u7570\u306A\u308B\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u3001\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\u9593\u3067\
  \u30C7\u30FC\u30BF\u3092\u7C21\u5358\u306B\u4EA4\u63DB\u3057\u305F\u308A\u3001\u30C7\
  \u30FC\u30BF\u51E6\u7406\u3084\u5206\u6790\u4F5C\u696D\u3092\u5BB9\u6613\u306B\u3059\
  \u308B\u305F\u3081\u306B\u3001CSV\u30D5\u30A1\u30A4\u30EB\u3092\u64CD\u4F5C\u3057\
  \u307E\u3059\u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 何となぜ？

CSV（カンマ区切り値）の操作とは、プレーンテキストで表形式のデータを保存する一般的なフォーマットであるCSVファイルからの読み取りと、それにデータを書き込むことを含みます。プログラマーは、異なるアプリケーション、データベース間でデータを簡単に交換したり、データ処理や分析作業を容易にするために、CSVファイルを操作します。

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
