---
title:                "Swift: 「CSVの取り扱い」"
simple_title:         "「CSVの取り扱い」"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜ

CSVを使用することによって、データの表形式での管理や分析が容易になります。Swiftを使用することで、より効率的かつ正確なデータ処理が可能になります。

## 方法

まず、CSVファイルを読み込むために、Foundationフレームワークの`CSV`クラスをインポートします。

```
import Foundation
```

次に、CSVファイルの場所を指定し、ファイルを`String`型の変数に割り当てます。

```
let filePath = "path/to/file.csv"
let csvString = try String(contentsOfFile: filePath)
```

`csvString`変数には、CSVファイルの内容が文字列として格納されます。

CSVのデータを分析するには、`csvString`をカンマや改行などの区切り文字で分割し、配列に変換します。その後、配列をループして必要なデータを取得することができます。

以下は、CSVファイルの内容をコンソールに出力する例です。

```
csvString.components(separatedBy: ", ").forEach { row in
    print(row)
}
```

もし、CSVファイルにヘッダー行がある場合は、配列の先頭を除外する必要があります。

```
let rows = csvString.components(separatedBy: ", ")
rows.dropFirst().forEach { row in
    print(row)
}
```

## 深堀り

CSVファイルを扱う際によく使用するのが、`CSVWriter`クラスです。これを使用することで、CSVファイルにデータを書き込むことができます。

以下は、`CSVWriter`を使用して新しいCSVファイルを作成し、データを書き込む例です。

```
let filePath = "path/to/new_file.csv"
let csvWriter = try CSVWriter(file: filePath, headers: ["Name", "Age", "Location"], delimiter: ",")
try csvWriter.write(row: ["John", "28", "Tokyo"])
try csvWriter.write(row: ["Emily", "32", "Osaka"])
```

この例では、CSVファイルに`Name`、`Age`、`Location`の列ヘッダーが追加され、それぞれの値が行として書き込まれます。

## 関連リンク

[Apple Developer Documentation - CSV](https://developer.apple.com/documentation/foundation/csv)

[Hacking with Swift - Working with CSV](https://www.hackingwithswift.com/example-code/strings/how-to-load-a-string-from-a-file-in-your-bundle)

[Swift CSV Parser](https://github.com/yaslab/CSV.swift)