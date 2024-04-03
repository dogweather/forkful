---
date: 2024-01-20 17:39:18.661902-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3053\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u4E00\u8CAB\u3057\u305F\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u306B\u3059\u308B\u884C\u70BA\u3067\u3059\u3002\u3053\
  \u308C\u306F\u5927\u6587\u5B57\u5C0F\u6587\u5B57\u3092\u533A\u5225\u3057\u306A\u3044\
  \u691C\u7D22\u3084\u30BD\u30FC\u30C8\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\
  \u6B63\u898F\u5316\u306E\u305F\u3081\u306B\u3088\u304F\u884C\u308F\u308C\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.593807-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3053\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u4E00\u8CAB\u3057\u305F\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u306B\u3059\u308B\u884C\u70BA\u3067\u3059\u3002\u3053\
  \u308C\u306F\u5927\u6587\u5B57\u5C0F\u6587\u5B57\u3092\u533A\u5225\u3057\u306A\u3044\
  \u691C\u7D22\u3084\u30BD\u30FC\u30C8\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\
  \u6B63\u898F\u5316\u306E\u305F\u3081\u306B\u3088\u304F\u884C\u308F\u308C\u307E\u3059\
  \u3002."
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

## What & Why? (何となぜ？)
文字列を小文字に変換することは、テキストを一貫したフォーマットにする行為です。これは大文字小文字を区別しない検索やソート、ユーザー入力の正規化のためによく行われます。

## How to: (やり方)
Swiftでは文字列を小文字にするのは簡単です。以下にコード例を示します。

```Swift
let originalString = "Hello, Swift Programmer!"
let lowercasedString = originalString.lowercased()

print(lowercasedString)
```

このコードは以下の結果を出力します。

```
hello, swift programmer!
```

## Deep Dive (深堀り)
Swiftでは、`lowercased()` メソッドが文字列の全ての文字を小文字に変換します。これはSwiftの初期バージョンから利用可能で、他の多くのプログラミング言語にも似た機能があります。

選択肢として、特定のロケール（言語や国）に基づいて小文字化を行うこともできます。例えば、トルコ語での"i"と"İ"の小文字化は英語とは異なります。デフォルトでは、`lowercased()`は現在のロケールを使用します。

実装の面では、Swiftの`lowercased()`はUnicodeをサポートしていて、世界中の様々なスクリプトの大文字小文字の変換に対応しています。

## See Also (関連情報)
- [Swift Documentation: Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Unicode Standard: Case Folding](https://unicode.org/reports/tr21/)
