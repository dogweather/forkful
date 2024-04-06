---
date: 2024-01-20 17:39:18.661902-07:00
description: "How to: (\u3084\u308A\u65B9) Swift\u3067\u306F\u6587\u5B57\u5217\u3092\
  \u5C0F\u6587\u5B57\u306B\u3059\u308B\u306E\u306F\u7C21\u5358\u3067\u3059\u3002\u4EE5\
  \u4E0B\u306B\u30B3\u30FC\u30C9\u4F8B\u3092\u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.398506-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Swift\u3067\u306F\u6587\u5B57\u5217\u3092\u5C0F\u6587\
  \u5B57\u306B\u3059\u308B\u306E\u306F\u7C21\u5358\u3067\u3059\u3002\u4EE5\u4E0B\u306B\
  \u30B3\u30FC\u30C9\u4F8B\u3092\u793A\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
