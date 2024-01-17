---
title:                "CSVを使用したプログラミング"
html_title:           "Swift: CSVを使用したプログラミング"
simple_title:         "CSVを使用したプログラミング"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## 何？なんで？
CSVを使うことは、表形式のデータを扱うことです。プログラマーがこれをする理由は、データを整理しやすくするためです。

## 方法：
```Swift
let str = "1,2,3"
let csvArray = str.split(separator: ",") // ["1", "2", "3"]
```

```Swift
let csvData = "name,age,gender\nJohn,25,Male\nJane,30,Female"
let rows = csvData.components(separatedBy: .newlines)
let headers = rows[0].components(separatedBy: ",") // ["name", "age", "gender"]
for row in rows[1...] {
    let columns = row.components(separatedBy: ",")
    let name = columns[0] // John
    let age = Int(columns[1]) // 25
    let gender = columns[2] // Male
}
```

## もっと詳しく：
CSVは、Comma-Separated Valuesの略で、1972年に開発されました。代わりにExcelやJSONなどのフォーマットがありますが、CSVは簡単で使いやすいことが多くのプログラマーに好まれています。Swiftでは、`String`や`NSArray`の便利なメソッドを使って、CSVのデータを処理することができます。

## 他にも：
CSVKitやPapaParseなどのライブラリがありますが、Swiftの標準ライブラリを使えば簡単にCSVを扱うことができます。また、CSVはWebサイトのスクレイピングやデータのエクスポートなどでもよく使われます。

## 関連リンク：
- [SwiftのStringのメソッド](https://developer.apple.com/documentation/swift/string)
- [CSVの詳しい説明](https://en.wikipedia.org/wiki/Comma-separated_values)