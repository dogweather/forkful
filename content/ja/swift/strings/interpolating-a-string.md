---
date: 2024-01-20 17:51:44.817858-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.799307-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u904E\u53BB\u3001\u6587\u5B57\u5217\u306F`+`\u6F14\u7B97\
  \u5B50\u3067\u7D50\u5408\u3055\u308C\u3066\u3044\u307E\u3057\u305F\u304C\u3001\u3053\
  \u308C\u306F\u7169\u96D1\u3067\u8AAD\u307F\u306B\u304F\u3044\u30B3\u30FC\u30C9\u306B\
  \u306A\u308A\u304C\u3061\u3067\u3057\u305F\u3002Swift\u304C\u5C0E\u5165\u3055\u308C\
  \u305F\u6642\u3001\u6587\u5B57\u5217\u88DC\u9593\u306F\u30B3\u30FC\u30C9\u306E\u660E\
  \u77AD\u3055\u3068\u7C21\u6F54\u3055\u3092\u5927\u5E45\u306B\u6539\u5584\u3057\u307E\
  \u3057\u305F\u3002Python\u3084Ruby\u306A\u3069\u4ED6\u306E\u8A00\u8A9E\u306B\u3082\
  \u4F3C\u305F\u6A5F\u80FD\u304C\u3042\u308A\u307E\u3059\u304C\u3001Swift\u306E\u6587\
  \u5B57\u5217\u88DC\u9593\u306F\u578B\u5B89\u5168\u3092\u5F37\u5316\u3059\u308B\u305F\
  \u3081\u3001\u30B3\u30F3\u30D1\u30A4\u30EB\u6642\u306B\u578B\u691C\u67FB\u304C\u884C\
  \u308F\u308C\u307E\u3059."
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
