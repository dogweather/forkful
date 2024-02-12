---
title:                "テキストの検索と置換"
aliases: - /ja/swift/searching-and-replacing-text.md
date:                  2024-01-20T17:58:52.589498-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
プログラムでテキストを検索・置換するのは、ある文字列を別の文字列に変更する処理です。データの正規化、不要な情報の削除、またはコードのリファクタリングなど、様々な状況でこの処理が必要です。

## How to (方法)
```Swift
var greeting = "Hello, world!"

// 文字列を置換する - replaceOccurrences(of:with:)
greeting = greeting.replacingOccurrences(of: "Hello", with: "Goodbye")
print(greeting)  // "Goodbye, world!"

// 正規表現を使って置換する
let htmlString = "<p>This is a paragraph.</p>"
let strippedString = htmlString.replacingOccurrences(of: "<.*?>", with: "", options: .regularExpression)
print(strippedString) // "This is a paragraph."
```

## Deep Dive (詳細情報)
テキストの検索と置換は、初期のコンピューターシステムから存在します。UNIXのsedコマンドやPerl言語は、この営為をよりパワフルで使いやすくしました。Swiftでは、`String`の`replacingOccurrences(of:with:)`メソッドを通じて簡単に置換ができます。正規表現を使えば、より複雑なパターン検索も可能です。ただし、正規表現は読みにくい、書きにくいという側面もあるため、単純な置換では通常の文字列メソッドを使った方が良いでしょう。

## See Also (関連情報)
- [Swift Standard Library - String](https://developer.apple.com/documentation/swift/string)
- [NSRegularExpression - Apple Developer](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Using Regular Expressions in Swift](https://www.raywenderlich.com/5765-regular-expressions-tutorial-getting-started)
