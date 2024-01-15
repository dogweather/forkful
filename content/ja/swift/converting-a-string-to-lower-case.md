---
title:                "文字列を小文字に変換する"
html_title:           "Swift: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換することに取り組む理由は主に2つあります。まず第一に、データの整合性を保つために、すべての文字列を同じ形式で処理する必要があります。また、データの検索や比較を行う際に、大文字と小文字を区別する必要がある場合があります。

## 使い方
下記のように、文字列を小文字に変換するには `lowercased()` メソッドを使用します。

```Swift
let str = "Hello, World!"
let lowercasedStr = str.lowercased()
print(lowercasedStr) // hello, world!
```

また、Unicode文字列にも適用することができます。

```Swift
let unicodeStr = "こんにちは、世界！"
let lowercaseUnicode = unicodeStr.lowercased()
print(lowercaseUnicode) // こんにちは、世界！
```

## 深堀り
Swiftでは、文字列を小文字に変換するために `UnicodeScalar`という型が使用されます。 `UnicodeScalar` は、ASCII文字列やUnicode文字列の一部を表すために使用される2バイトの整数です。 `lowercased()` メソッドは、この `UnicodeScalar` を使用して文字列を小文字に変換します。これにより、大文字と小文字の区別を考慮して、文字列を安全かつ簡潔に変換することができます。

## はじめよう
Swiftで文字列を小文字に変換する方法について学びました。あなたのプロジェクトで文字列を処理する際は、常に文字列を同じ形式に変換することを忘れないようにしてください。

## 参考
- [Swift Documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Apple Developer Documentation](https://developer.apple.com/documentation/swift)
- [Stack Overflow](https://stackoverflow.com/questions/3214565/how-do-i-convert-a-string-to-lower-case-in-swift)