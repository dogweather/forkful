---
title:                "Kotlin: 「CSVを扱う方法」"
simple_title:         "「CSVを扱う方法」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを使用するのか？

CSV（Comma Separated Values）は、データをより簡単に取り扱うためのファイル形式です。多くのアプリケーションやプログラミング言語でサポートされており、データの表形式での取り込みや出力に適しています。KotlinではCSVを扱うための多くのライブラリがあり、便利に使用することができます。

## 使い方

CSVファイルを読み込むには、まずはライブラリをインポートします。

```Kotlin
import com.github.doyaaaaaken.kotlincsv.dsl.csvReader
```

そして、以下のようにファイルのパスを指定してcsvReaderを呼び出します。

```Kotlin
csvReader().readAll("/path/to/file.csv")
```

もしヘッダーがあるファイルの場合は、次のようにheaderというオプションを指定することでヘッダーを取得することもできます。

```Kotlin
csvReader().readAllWithHeader("/path/to/file.csv")
```

CSVファイルを作成するには、以下のようにデータをリストとして渡し、ファイル名を指定します。

```Kotlin
val data = listOf(
  listOf("Name", "Age", "Gender"),
  listOf("John", "25", "Male"),
  listOf("Lisa", "30", "Female")
)

csvWriter().writeAll(data, "/path/to/new_file.csv")
```

作成されたCSVファイルは、テキストエディタやスプレッドシートソフトで開いて編集することも可能です。

## 詳細を深める

CSVファイルの取り扱い方についてさらに深く知りたい方は、以下のリンクを参考にしてください。

- [Kotlin CSVライブラリドキュメント](https://github.com/doyaaaaaken/kotlincsv)
- [CSVの仕様詳細](https://tools.ietf.org/html/rfc4180)
- [CSVを扱う際の注意点](https://www.e-typing.ne.jp/word/civ/knowledge-about-csv/)

## その他の参考リンク

- [Markdown記法の使い方](https://qiita.com/tbpgr/items/989c6badefff69377da7)
- [Kotlin公式サイト](https://kotlinlang.org/)