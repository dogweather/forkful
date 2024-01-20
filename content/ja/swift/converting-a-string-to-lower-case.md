---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何故 & 何のために?
文字列を小文字に変換するとは、すべての大文字をその対応する小文字に置き換えることを意味します。プログラマは、文字列の比較を行う際などに、大文字と小文字を区別しない「ケースインセンシティブ」な検索を実行するためにこれを行います。

## どうやって:
Swiftには文字列を小文字に変換するための内蔵メソッド `lowercased()` があります。以下はその使用例です:

```Swift
let originalString = "HELLO, SWIFT!"
let lowercasedString = originalString.lowercased()

print(lowercasedString)
```

実行結果は以下の通りです:

```Swift
"hello, swift!"
```

## ディープダイブ:
文字列を小文字に変換するという操作は、大昔の時代から存在しています。様々なプログラミング言語で採用されており、存在する文字セットやその言語の文字列操作の仕組みに基づいて異なった実装がなされています。

例えばJavaScriptなど、他の言語では、同様の操作を行うための別のメソッドや関数が存在します。

Swiftでは、`lowercased()`は文字列から新たな小文字の文字列を返す非変異メソッドです。文字列自体は変更されません。これにより、未意図的なサイドエフェクトを避けることができます。

## 参考資料:
以下では、Stringクラス、`lowercased()`メソッド、およびSwiftの文字列操作に更について学ぶことができます:

- [Apple Developer Documentation: String](https://developer.apple.com/documentation/swift/string)
- [Swift Programming Guide: Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)