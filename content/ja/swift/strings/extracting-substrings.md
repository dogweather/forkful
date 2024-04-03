---
date: 2024-01-20 17:46:45.375012-07:00
description: "How to: (\u65B9\u6CD5) Swift\u3067\u306F\u3001`String`\u578B\u304B\u3089\
  \u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u3092\u53D6\u308A\u51FA\u3059\u305F\u3081\
  \u306B\u3001`startIndex`, `endIndex`, `range`\u3092\u4F7F\u3044\u307E\u3059\u3002\
  \u4E0B\u8A18\u306F\u30B3\u30FC\u30C9\u4F8B\u3068\u51FA\u529B\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.596876-06:00'
model: gpt-4-1106-preview
summary: "Swift\u3067\u306F\u3001`String`\u578B\u304B\u3089\u30B5\u30D6\u30B9\u30C8\
  \u30EA\u30F3\u30B0\u3092\u53D6\u308A\u51FA\u3059\u305F\u3081\u306B\u3001`startIndex`,\
  \ `endIndex`, `range`\u3092\u4F7F\u3044\u307E\u3059\u3002\u4E0B\u8A18\u306F\u30B3\
  \u30FC\u30C9\u4F8B\u3068\u51FA\u529B\u3067\u3059."
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to: (方法)
Swiftでは、`String`型からサブストリングを取り出すために、`startIndex`, `endIndex`, `range`を使います。下記はコード例と出力です。

```Swift
let text = "こんにちは世界"
let startIndex = text.index(text.startIndex, offsetBy: 5)
let endIndex = text.index(text.startIndex, offsetBy: 7)
let substring = text[startIndex...endIndex]  // 世界
print(substring)  // 出力: "世界"
```

`startIndex`と`endIndex`を使って範囲を指定する方法もあります。

```Swift
let range = startIndex...endIndex
let substringRange = text[range]  // 世界
print(substringRange)  // 出力: "世界"
```

## Deep Dive (掘り下げ)
サブストリングはSwiftの初期バージョンからありますが、安全性と効率の観点で何度か改善されてきました。`NSString`から引き継いだAPIも使用可能ですが、`String`型のAPIの方がSwiftらしく、扱いやすいです。サブストリングは元の文字列のメモリを共有するため、大きな文字列から小さなサブストリングを取り出す際もメモリ効率が良いです。ただし、サブストリングは長時間保持するには向かないため、必要があれば`String`に変換することを忘れずに。

## See Also (関連する情報)
- [The Swift Programming Language (Swift 5.6) - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Using Swift’s Substring](https://www.hackingwithswift.com/example-code/strings/using-swifts-substring)

以上がサブストリングの基本的な取り扱い方です。適切に使用すれば、プログラムの柔軟性と効率が上がります。
