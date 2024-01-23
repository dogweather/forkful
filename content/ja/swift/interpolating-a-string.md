---
title:                "文字列の補間"
date:                  2024-01-20T17:51:44.817858-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間とは、文字列に変数や定数、リテラル、式を埋め込むこと。プログラマは、動的な文字列を作ったり、読みやすい出力を生成したりするためにこれを行います。

## How to: (方法)
```swift
// 文字列補間の基本
let name = "田中"
let greeting = "こんにちは、\(name)さん!"
print(greeting)  // 出力: こんにちは、田中さん!

// 式の利用
let price = 1500
let taxRate = 0.08
let totalPrice = "合計: \(Double(price) * (1 + taxRate))円"
print(totalPrice)  // 出力: 合計: 1620.0円

// 複数の値の補間
let quantity = 3
let item = "りんご"
let summary = "\(quantity)個の\(item)を買いました。"
print(summary)  // 出力: 3個のりんごを買いました。
```

## Deep Dive (深い潜水)
過去、文字列は`+`演算子で結合されていましたが、これは煩雑で読みにくいコードになりがちでした。Swiftが導入された時、文字列補間はコードの明瞭さと簡潔さを大幅に改善しました。PythonやRubyなど他の言語にも似た機能がありますが、Swiftの文字列補間は型安全を強化するため、コンパイル時に型検査が行われます。

裏では、Swiftは補間された各部分を取り、それらを適切な文字列表現に変換して1つの新しい文字列に結合します。これにより、パフォーマンスの低下を防ぎつつ、動的な文字列の生成が可能になります。 

文字列補間は単なる変数埋め込みにとどまらず、\()内で計算や関数呼び出しも行えます。これにより、必要に応じて複雑なロジックも展開できる強力なツールです。

## See Also (関連項目)
- Swift公式ドキュメント内の文字列補間のセクション: [String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
