---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？

部分文字列の抽出は、文字列から特定の部分を切り取ることです。これは通常、データをさまざまな形式に短縮したり、特定の値を見つけるために行われます。

## やり方：

Swiftでは、文字列から部分文字列を抽出する方法はいくつかあります。主に以下の2つの方法があります。

1. Swiftの `substring` メソッドを使用する

```Swift
let str = "こんにちは世界"
let indexStart = str.index(str.startIndex, offsetBy: 0)
let indexEnd = str.index(str.startIndex, offsetBy: 4)
let substring = str[indexStart...indexEnd]
print(substring)
```

出力： `こんにちは`

2. Swiftの `prefix` と `suffix` メソッドを使用する

```Swift 
let str = "こんにちは世界"
let firstThree = str.prefix(3)
let lastThree = str.suffix(3)
print(firstThree)
print(lastThree)
```
出力：
`こんに`
`は世界`

## ディープダイブ：

部分文字列の抽出は、Swiftが最初に登場した時から存在しています。これは、多くのプログラムでデータ解析と操作に不可欠な機能であり、時間とともに改善と最適化が図られてきました。

代替方法として[Regular Expressions](https://developer.apple.com/documentation/foundation/nsregularexpression)を使用して文字列を解析する方法がありますが、その複雑さからSwiftではあまり好まれていません。

抽出された部分文字列は、元の文字列と同じストレージを共有するため、メモリ効率が非常に高いです。したがって、大きな文字列から小さな部分文字列を抽出しても、新しいメモリを消費することなく行うことができます。

## 参照：

関連する情報は、以下のリンクを参照してください。

- [Apple's Swift Documentation Substrings](https://developer.apple.com/documentation/swift/substring)
- [Swift Standard Library String methods](https://developer.apple.com/documentation/swift/string)