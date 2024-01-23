---
title:                "文字列を小文字に変換"
date:                  2024-01-20T17:39:18.661902-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

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
