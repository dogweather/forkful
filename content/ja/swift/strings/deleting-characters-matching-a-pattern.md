---
date: 2024-01-20 17:43:22.958736-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\
  \u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u3053\u3068\
  \u306F\u3001\u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3084\u4E0D\u8981\u306A\u60C5\u5831\
  \u306E\u9664\u53BB\u306B\u5F79\u7ACB\u3061\u307E\u3059\u3002\u30AF\u30EA\u30FC\u30F3\
  \u306A\u30C7\u30FC\u30BF\u306F\u52B9\u7387\u7684\u306A\u51E6\u7406\u3084\u5206\u6790\
  \u306E\u571F\u53F0\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.590013-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\
  \u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u3053\u3068\
  \u306F\u3001\u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3084\u4E0D\u8981\u306A\u60C5\u5831\
  \u306E\u9664\u53BB\u306B\u5F79\u7ACB\u3061\u307E\u3059\u3002\u30AF\u30EA\u30FC\u30F3\
  \u306A\u30C7\u30FC\u30BF\u306F\u52B9\u7387\u7684\u306A\u51E6\u7406\u3084\u5206\u6790\
  \u306E\u571F\u53F0\u3067\u3059\u3002."
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

## How to: (方法)
```Swift
import Foundation

let originalString = "Hello, 2023! Happy New Year!"
let pattern = "[0-9,!]"
let regex = try! NSRegularExpression(pattern: pattern, options: [])
let range = NSRange(location: 0, length: originalString.utf16.count)
let cleanString = regex.stringByReplacingMatches(in: originalString, options: [], range: range, withTemplate: "")

print(cleanString) // "Hello Happy New Year"
```
サンプル出力は `Hello Happy New Year` になります。

## Deep Dive (詳細な解説)
文字の削除は多くのプログラムで必要とされるため、Swiftにはこの機能をサポートする文字列操作のメソッドが組み込まれています。古くはUNIXセド（sed）などのツールで行われてきましたが、Swiftでは`NSRegularExpression`を使うことで柔軟かつ強力なパターンマッチングが可能です。`NSRegularExpression`では、正規表現を使用してパターンを定義し、一致する文字列を見つけて操作を行います。一方で、よりシンプルな文字列操作が必要な場合、`String`の`replacingOccurrences(of:with:)`メソッドを使用する選択肢もあります。しかし、これはより単純な置換に限定され、正規表現のような柔軟性や強力なパターンマッチング機能は提供しません。

## See Also (関連情報)
- Swift公式ドキュメント: [String](https://developer.apple.com/documentation/swift/string)
- NSRegularExpressionクラスリファレンス: [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- 正規表現の基本: [Regular Expressions Quick Start](https://www.regular-expressions.info/quickstart.html)
- 文字列操作についてのチュートリアル: [RayWenderlich.com](https://www.raywenderlich.com/5539282-nsregularexpression-tutorial-for-swift)
