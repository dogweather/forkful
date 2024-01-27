---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSVはデータのシンプルな形式で、プログラマはデータ交換や簡単な保存に使います。

## How to:
```kotlin
import java.io.File

// CSVファイルを読む
fun readCsv(fileName: String) {
    val data = mutableListOf<List<String>>()
    File(fileName).forEachLine { line ->
        val row = line.split(",")
        data.add(row)
    }
    println(data)
}

// CSVファイルに書く
fun writeCsv(fileName: String, data: List<List<String>>) {
    File(fileName).bufferedWriter().use { writer ->
        data.forEach { row ->
            writer.write(row.joinToString(",") + "\n")
        }
    }
}

val csvData = listOf(
    listOf("id", "名前", "年"),
    listOf("1", "山田太郎", "25"),
    listOf("2", "鈴木一郎", "30")
)

val fileName = "example.csv"
writeCsv(fileName, csvData)
readCsv(fileName)
```
実行結果:
```
[[id, 名前, 年], [1, 山田太郎, 25], [2, 鈴木一郎, 30]]
```

## Deep Dive
CSV（Comma-Separated Values）は1970年代からある。より軽量で人が読める代わりにJSONやXMLがある。Kotlinでは標準ライブラリだけでなく、kotlinx.serializationやApache Commons CSVなどのライブラリで扱える。

## See Also
- Kotlin 公式ドキュメンテーション: https://kotlinlang.org/docs/home.html
- kotlinx.serialization: https://github.com/Kotlin/kotlinx.serialization
- Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
