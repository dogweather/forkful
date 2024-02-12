---
title:                "パターンに一致する文字を削除する"
aliases:
- /ja/swift/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:22.958736-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から特定のパターンに一致する文字を削除することは、データの整形や不要な情報の除去に役立ちます。クリーンなデータは効率的な処理や分析の土台です。

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
