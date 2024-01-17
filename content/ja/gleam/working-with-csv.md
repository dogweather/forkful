---
title:                "CSVファイルを扱う"
html_title:           "Gleam: CSVファイルを扱う"
simple_title:         "CSVファイルを扱う"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## CSVとは？
CSVとは、プログラマーがデータをテキスト形式で保存・交換するためのファイル形式です。各行が列（カラム）に分かれ、データがコンマで区切られています。プログラムでCSVを使う理由は、データを簡単に扱うことができる上に、様々なアプリケーション間でのデータ共有が容易になるからです。

## 試してみる：
CSVファイルをGleamで処理する方法を説明します。まずは、Gleamの標準ライブラリである```csv```モジュールをインポートします。次に、```csv.from_string()```関数を使ってCSV形式のデータを文字列として読み込みます。最後に、文字列に対して必要な処理を行い、データを取り出します。

```gleam
import csv

fn main() {
  let csv_string = "name,age,city\nJohn,30,Tokyo\nEmily,25,New York\n"

  let csv_data = csv.from_string(csv_string)

  for row in csv_data do
    let name = row.get(0)
    let age = row.get(1)
    let city = row.get(2)
    io.println("Name: {}, Age: {}, City: {}", [name, age, city])  // 出力：Name: John, Age: 30, City: Tokyo
                                                                  // 出力：Name: Emily, Age: 25, City: New York
}
```

## 詳しく見てみる：
CSVが登場したのは1970年代で、表計算ソフトでのデータ取り扱いのために開発されました。その後、インターネットの爆発的な普及に伴い、データのやりとりにおいても重要なフォーマットとなりました。CSV以外にも、ExcelやJSONなどのデータ形式がありますが、CSVはどのアプリケーションでも簡単に扱えるため、非常に人気があります。

## 参考文献：
- [GleamのCSVモジュールドキュメント (英語)](https://gleam.run/modules/csv/)
- [CSVの歴史 (英語)](https://digitalassetmanagementnews.org/developers/a-brief-history-of-csv/)
- [CSVとは - Wikipedia (日本語)](https://ja.wikipedia.org/wiki/%E3%83%9D%E3%83%BC%E3%82%BF%E3%83%96%E3%83%AB%E4%BD%8D%E3%81%95%E3%82%8C%E3%81%A6%E3%81%84%E3%82%8B%E3%83%87%E3%83%BC%E3%82%BF%E3%83%99%E3%83%BC%E3%82%B9)