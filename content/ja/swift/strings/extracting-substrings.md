---
date: 2024-01-20 17:46:45.375012-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u90E8\u5206\u3092\u53D6\
  \u308A\u51FA\u3059\u3053\u3068\u3092\u300C\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\
  \u300D\u3068\u3044\u3044\u307E\u3059\u3002\u30C7\u30FC\u30BF\u3092\u89E3\u6790\u3057\
  \u305F\u308A\u3001\u7279\u5B9A\u306E\u60C5\u5831\u3060\u3051\u3092\u8868\u793A\u3057\
  \u305F\u308A\u3059\u308B\u3068\u304D\u306B\u5FC5\u8981\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.596876-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u90E8\u5206\u3092\u53D6\
  \u308A\u51FA\u3059\u3053\u3068\u3092\u300C\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\
  \u300D\u3068\u3044\u3044\u307E\u3059\u3002\u30C7\u30FC\u30BF\u3092\u89E3\u6790\u3057\
  \u305F\u308A\u3001\u7279\u5B9A\u306E\u60C5\u5831\u3060\u3051\u3092\u8868\u793A\u3057\
  \u305F\u308A\u3059\u308B\u3068\u304D\u306B\u5FC5\u8981\u3067\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から特定の部分を取り出すことを「サブストリング」といいます。データを解析したり、特定の情報だけを表示したりするときに必要です。

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
