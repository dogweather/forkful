---
title:                "「CSVデータを扱う」"
html_title:           "Swift: 「CSVデータを扱う」"
simple_title:         "「CSVデータを扱う」"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜ

CSVを扱うのに取り組む理由は、多様なデータを扱う必要がある場合に、データを簡単かつ効率的に読み書きするためです。

## 手順

まず、CSVファイルを読み込むためのライブラリをインポートします。次に、`Reader`オブジェクトを作成し、CSVファイルを読み込みます。`Reader`オブジェクトの`next()`メソッドを使用することで、1行ずつデータを読み込むことができます。また、`CharacterSet`を使用して、データをカンマで区切ることができます。以下は、`CSVファイルの読み込み例です。

```Swift
import Foundation
import CSV

let reader = try! CSVReader(url: URL(fileURLWithPath: "data.csv"))
while let row = reader.next() {
    print(row)
}

```
```
["John", "Doe", "30"]
["Jane", "Smith", "25"]
```

CSVファイルを書き込む場合、`Writer`オブジェクトを作成し、`write(row:)`メソッドを使用して1行ずつデータを書き込むことができます。また、データをカンマで区切ることも可能です。以下は、`CSVファイルの書き込み例です。

```Swift
import Foundation
import CSV

let writer = try! CSVWriter(url: URL(fileURLWithPath: "data.csv"))

try? writer.write(row: ["John", "Doe", "30"])
try? writer.write(row: ["Jane", "Smith", "25"])
```

## 深堀り

CSVファイルを読み込む際には、文字エンコーディングやセルのエスケープ処理などにも注意する必要があります。`Reader`オブジェクトの初期化時には、`encoding`パラメータを指定することで、読み込むファイルのエンコーディングを指定できます。また、デフォルトではカンマの他にもタブやセミコロンなど、複数の区切り文字に対応しています。さらに、CSVファイルをパースする際には、空白文字や特別な文字をエスケープする必要がある場合があります。そのため、`escapeStrategy`パラメータを使用して、エスケープ処理の方法をカスタマイズすることができます。

## See Also

- [CSVクラスのドキュメント](https://developer.apple.com/documentation/foundation/csv)
- [OpenCSVライブラリのGitHubリポジトリ](https://github.com/davedelong/CSV)