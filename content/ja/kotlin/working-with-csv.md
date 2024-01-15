---
title:                "「csvを扱う」"
html_title:           "Kotlin: 「csvを扱う」"
simple_title:         "「csvを扱う」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを使用するのか

CSVはコンマで区切られたデータを表すためのファイル形式です。多くのアプリケーションやプログラムで使用されており、データのやりとりや処理に役立ちます。Kotlinを使用してCSVを処理することで、より効率的にデータを扱うことができます。

## 方法

KotlinでCSVを処理するためには、最初に**kotlin-csv**ライブラリをインストールする必要があります。

```
// Gradle
implementation 'com.github.doyaaaaaken:kotlin-csv:1.0.4'

// Maven
<dependency>
    <groupId>com.github.doyaaaaaken</groupId>
    <artifactId>kotlin-csv</artifactId>
    <version>1.0.4</version>
</dependency>
```

次に、CSVファイルを読み込みたい場合は、`FileReader`クラスを使用します。

```kotlin
val reader: FileReader<File> = FileReader("data.csv")
val csvParser: CsvReader = CsvReader()
val rows: List<CsvRow> = csvParser.parseAll(reader)
```

データを処理する方法は多数ありますが、例として`for`ループを使用して各行のデータを出力する方法を紹介します。

```kotlin
for (row in rows) {
    println(row[0] + "さんは" + row[1] + "歳です。")
}
```

出力結果は以下のようになります。

```
山田さんは30歳です。
田中さんは25歳です。
佐藤さんは28歳です。
```

## ディープダイブ

CSVファイルを処理する際に注意すべきことは、各セルのデータ型です。全てが文字列として扱われるため、数値や日付のような特定の形式を持つデータを正しく処理するためには、適切なデータ型に変換する必要があります。また、CSVファイルが大きなサイズを持つ場合は、メモリ不足の可能性がありますので、`CsvReader`の代わりに`CsvReaderIterator`クラスを使用することで、メモリの消費量を抑えることができます。

## 参考リンク

[kotlin-csvライブラリ](https://github.com/doyaaaaaken/kotlin-csv)

[FileReaderクラスのドキュメント](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file-reader/index.html)

[CsvReaderクラスのドキュメント](https://kotlin.github.io/kotlinx-datetime/java-time-to-kotlin-times/#csv-io)