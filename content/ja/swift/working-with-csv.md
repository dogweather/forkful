---
title:                "CSVファイルの操作"
date:                  2024-01-19
simple_title:         "CSVファイルの操作"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
"## なぜCSV？"
CSVとは、コンマで区切られた値のデータ形式です。データの移行や分析に使われ、シンプルで理解しやすい構造が特徴です。

## How to:
"## 実装方法"
SwiftでCSVを扱う一例です。以下のコードは、CSVデータを読み込んでパースします。

```Swift
import Foundation

let csvString = """
Name,Age,Location
John Doe,29,New York
Jane Smith,34,San Francisco
"""

var rows: [[String]] = []
csvString.enumerateLines { line, _ in
    let columns = line.components(separatedBy: ",")
    rows.append(columns)
}

for row in rows {
    print(row)
}
```

実行すると、このような出力が得られます。

```
["Name", "Age", "Location"]
["John Doe", "29", "New York"]
["Jane Smith", "34", "San Francisco"]
```

## Deep Dive:
"## 掘り下げ情報"
CSVは、初期のコンピュータ時代からデータ交換の手段として使われています。JSONやXMLなどの代替手段もありますが、CSVはシンプルさと人間の可読性において優れています。Swiftでは、`String`クラスを使って簡単にパース可能ですが、大規模データや複雑な処理の場合は、専用のライブラリを使用するのが適切です。

## See Also:
"## 参考リンク"
- Swift公式ドキュメント: https://swift.org/documentation/
- CSVデータに関するRFC 4180: https://tools.ietf.org/html/rfc4180
- Swiftで使えるCSVパーサライブラリ: https://github.com/yaslab/CSV.swift

以上で、SwiftでCSVを扱う方法について紹介しました。もっと深く知りたい場合は、上記リンクを参照してください。
