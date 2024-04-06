---
date: 2024-01-20 17:58:52.589498-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.095095-06:00'
model: gpt-4-1106-preview
summary: "with:)`\u30E1\u30BD\u30C3\u30C9\u3092\u901A\u3058\u3066\u7C21\u5358\u306B\
  \u7F6E\u63DB\u304C\u3067\u304D\u307E\u3059\u3002\u6B63\u898F\u8868\u73FE\u3092\u4F7F\
  \u3048\u3070\u3001\u3088\u308A\u8907\u96D1\u306A\u30D1\u30BF\u30FC\u30F3\u691C\u7D22\
  \u3082\u53EF\u80FD\u3067\u3059\u3002\u305F\u3060\u3057\u3001\u6B63\u898F\u8868\u73FE\
  \u306F\u8AAD\u307F\u306B\u304F\u3044\u3001\u66F8\u304D\u306B\u304F\u3044\u3068\u3044\
  \u3046\u5074\u9762\u3082\u3042\u308B\u305F\u3081\u3001\u5358\u7D14\u306A\u7F6E\u63DB\
  \u3067\u306F\u901A\u5E38\u306E\u6587\u5B57\u5217\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\
  \u3063\u305F\u65B9\u304C\u826F\u3044\u3067\u3057\u3087\u3046\u3002"
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
