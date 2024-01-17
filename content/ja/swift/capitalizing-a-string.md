---
title:                "文字列の大文字化"
html_title:           "Swift: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

文字列の大文字化とは何かを説明し、プログラマーがなぜそれを行うのかを2〜3文で説明します。

## 方法：

文字列を大文字化する方法と、その結果を示す```Swift ... ```コードブロック内のコーディング例を示します。

```Swift
let name = "hello world"
let capitalizedName = name.uppercased()
print(capitalizedName) // Output: HELLO WORLD
```

## 詳細を掘り下げる：

文字列の大文字化に関する歴史的な文脈や、代替方法、実装の詳細など、より深い情報を提供します。

大文字化はいつでもオプションですが、特に英語と他の言語の大文字化を区別する必要がある場合に役立ちます。また、大文字と小文字の区別が重要なプログラミング言語では、文字列の大文字化は重要です。

```Swift
//代替方法
let name = "Hello World"
let capitalizedName = name.capitalized // Output: Hello World

//実装の詳細
public func uppercased() -> String { ... }
//全文字を大文字に変換します。

public func capitalized() -> String { ... }
//最初の文字またはすべての単語の最初の文字を大文字に変換します。
```

## 関連情報：

文字列の大文字化に関する関連情報を提供するリンクを掲載します。

- [Swift Documentation - String Basics](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [String Manipulation in Swift](https://www.raywenderlich.com/267-string-manipulation-in-swift)
- [How to Capitalize Strings in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-strings-using-capitalize)