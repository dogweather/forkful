---
date: 2024-01-20 17:58:52.589498-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.396494-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

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
