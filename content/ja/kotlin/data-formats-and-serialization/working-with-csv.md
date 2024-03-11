---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:45.600557-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.674402-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "CSV\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

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
