---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:44.334543-07:00
description: "\u65B9\u6CD5 Swift\u3067\u306F\u3001CSV\u30D5\u30A1\u30A4\u30EB\u3092\
  \u76F4\u63A5\u89E3\u6790\u3059\u308B\u305F\u3081\u306E\u30CD\u30A4\u30C6\u30A3\u30D6\
  \u30B5\u30DD\u30FC\u30C8\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001`String`\u30E1\
  \u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u3066\u5185\u5BB9\u3092\u5206\u5272\u3059\
  \u308B\u304B\u3001SwiftCSV\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\
  \u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5229\u7528\u3057\u3066CSV\u30C7\
  \u30FC\u30BF\u3092\u6271\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\
  \u4E0B\u306B\u4E21\u65B9\u306E\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.441365-06:00'
model: gpt-4-0125-preview
summary: ''
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 方法
Swiftでは、CSVファイルを直接解析するためのネイティブサポートはありませんが、`String`メソッドを使用して内容を分割するか、SwiftCSVのようなサードパーティライブラリを利用してCSVデータを扱うことができます。以下に両方の方法を示します：

### 外部ライブラリなしの手動解析
```swift
// 単純なCSV文字列を考える
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// CSV文字列を行に分割する
let rows = csvString.components(separatedBy: "\n")

// 最初の行からキーを抽出する
let keys = rows.first?.components(separatedBy: ",")

// 2番目から始まる行を繰り返し処理する
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// サンプル出力
print(result)
// 出力：[{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
このアプローチは直接的ですが、値内のカンマ、フィールド内の改行など、特殊なケースを含むCSVファイルを扱う場合には堅牢性に欠けます。

### SwiftCSVライブラリを使用する
まず、`Package.swift`の依存関係にSwiftCSVを含めることによって、プロジェクトにSwiftCSVを追加します：
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
その後、以下のようにインポートして使用します：
```swift
import SwiftCSV

// `csvString`が上記のように定義されていると仮定

// CSVオブジェクトを作成
if let csv = try? CSV(string: csvString) {
    // 辞書として行にアクセス
    let rows = csv.namedRows
    
    // サンプル出力
    print(rows)
    // 出力：[{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSVは、カンマを分離する、フィールド内の改行、文字コーディングのような特有の問題を自動的に扱うことによって、解析を単純化します。しかし、外部データソースを扱う際、特にエラーを処理することを忘れないでください。
