---
title:                "文字列の切り出し"
html_title:           "Swift: 文字列の切り出し"
simple_title:         "文字列の切り出し"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列から部分文字列を抽出することの利点は多岐にわたります。例えば、文字列の特定の一部分を抽出して、より短く読みやすい文字列を作成したり、特定の条件に基づいて文字列を分割したりすることができます。

## 抽出方法

```Swift
let myString = "Hello, world!"
let substring = myString[..<5]

print(substring) // Hello
```

この例では、文字列の先頭から5文字までの部分文字列を抽出しています。 `..<`演算子を使用することで、抽出したい部分の範囲を指定することができます。また、インデックス番号を使用することで、特定の位置から始まる部分文字列を抽出することもできます。

```Swift
let myString = "Hello, world!"
let substring = myString[6...]

print(substring) // world!
```

さらに、条件に応じて文字列を分割することもできます。例えば、コロンで区切られた特定のワードのみを抽出することができます。

```Swift
let myString = "I love:Swift:programming:language"
let substrings = myString.split(separator: ":", omittingEmptySubsequences: false)

print(substrings) // ["I love", "Swift", "programming", "language"]
```

## 詳しく見る

文字列から部分文字列を抽出する方法は一見単純に見えますが、実は内部的には複雑な処理が行われています。文字列は`String`型であり、Swiftでは`Substring`型と呼ばれる部分文字列を抽出するための専用のデータ型が使用されています。これにより、多くのメモリを消費することなく、効率的に文字列を抽出することができます。

## 参考リンク

- [Swift Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift Substrings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID576)
- [Using Substrings in Swift](https://useyourloaf.com/blog/using-substrings-in-swift-4/)