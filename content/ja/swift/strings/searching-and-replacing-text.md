---
aliases:
- /ja/swift/searching-and-replacing-text/
date: 2024-01-20 17:58:52.589498-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\u30C6\u30AD\u30B9\u30C8\u3092\u691C\
  \u7D22\u30FB\u7F6E\u63DB\u3059\u308B\u306E\u306F\u3001\u3042\u308B\u6587\u5B57\u5217\
  \u3092\u5225\u306E\u6587\u5B57\u5217\u306B\u5909\u66F4\u3059\u308B\u51E6\u7406\u3067\
  \u3059\u3002\u30C7\u30FC\u30BF\u306E\u6B63\u898F\u5316\u3001\u4E0D\u8981\u306A\u60C5\
  \u5831\u306E\u524A\u9664\u3001\u307E\u305F\u306F\u30B3\u30FC\u30C9\u306E\u30EA\u30D5\
  \u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u306A\u3069\u3001\u69D8\u3005\u306A\u72B6\u6CC1\
  \u3067\u3053\u306E\u51E6\u7406\u304C\u5FC5\u8981\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.215746
model: gpt-4-1106-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\u30C6\u30AD\u30B9\u30C8\u3092\u691C\
  \u7D22\u30FB\u7F6E\u63DB\u3059\u308B\u306E\u306F\u3001\u3042\u308B\u6587\u5B57\u5217\
  \u3092\u5225\u306E\u6587\u5B57\u5217\u306B\u5909\u66F4\u3059\u308B\u51E6\u7406\u3067\
  \u3059\u3002\u30C7\u30FC\u30BF\u306E\u6B63\u898F\u5316\u3001\u4E0D\u8981\u306A\u60C5\
  \u5831\u306E\u524A\u9664\u3001\u307E\u305F\u306F\u30B3\u30FC\u30C9\u306E\u30EA\u30D5\
  \u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u306A\u3069\u3001\u69D8\u3005\u306A\u72B6\u6CC1\
  \u3067\u3053\u306E\u51E6\u7406\u304C\u5FC5\u8981\u3067\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
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
