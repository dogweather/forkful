---
title:                "正規表現の使用"
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
正規表現とは、文字列のパターンマッチングを行うための強力なツールです。プログラマーはテキストデータの検索、置換、解析のために使い、効率的な文字処理を実現します。

## How to:

Swiftで正規表現を使うには、`NSRegularExpression`クラスを活用します。以下に例を示します。

```Swift
import Foundation

let string = "swift@example.com"
let pattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}"

if let regex = try? NSRegularExpression(pattern: pattern) {
    let range = NSRange(location: 0, length: string.utf16.count)
    if regex.firstMatch(in: string, range: range) != nil {
        print("メールアドレスが有効です。")
    } else {
        print("メールアドレスが無効です。")
    }
}
```
出力: `メールアドレスが有効です。`

## Deep Dive

正規表現は、1960年代に初めて導入されました。Swift言語においては、`NSRegularExpression`クラスを通して利用可能となっていますが、これは元々Objective-Cのクラスです。代替として文字列メソッドもありますが、複雑なパターンには正規表現が適しています。`NSRegularExpression`ではキャプチャグループ、ルックアラウンドなど高度なマッチングも可能です。

## See Also

- Appleの`NSRegularExpression`ドキュメント:
    [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- Swift公式ドキュメント:
    [Swift.org](https://www.swift.org/documentation/)
- 正規表現を学ぶ:
    [正規表現101](https://regex101.com/)
- 無料のコード学習リソース:
    [Codecademy](https://www.codecademy.com/learn/learn-regex)