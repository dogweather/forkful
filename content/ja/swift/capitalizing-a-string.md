---
title:    "Swift: 文字列の頭文字を大文字に変換する"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字化するメリットや、それを実装することで何が得られるかについて説明します。

文字列を大文字化することで、より読みやすいテキストを作成することができます。例えば、ユーザーによる入力を受け取った際に、大文字で入力された文字列を正しいフォーマットに整えることができます。また、ゲームやアプリケーションでの名称の表示など、視覚的に区別する必要がある場合にも重要です。

## 方法

```Swift
let myString = "hello, world!"
let capitalizedString = myString.capitalized
print(capitalizedString) // Hello, World!
```

上記のコードを実行すると、文字列を大文字化することができます。まず、`myString`という変数に対して、`capitalized`というプロパティを使用し、大文字化した文字列を`capitalizedString`という新しい変数に代入しています。そして、`print()`関数を使用して大文字化された文字列を表示しています。

文字列を大文字化するだけでなく、特定の単語だけを大文字化することもできます。例えば、`myString.capitalizingFirstLetter()`というメソッドを使用することで、最初の文字を大文字化することができます。

## 深堀

文字列を大文字化する際に、英語以外の言語でも同様に動作することができます。この機能はSwiftの国際化（i18n）の一部として開発されており、様々な言語や地域に対応しています。

また、Swiftでは文字列を扱う際にUnicodeをサポートしており、他の言語や文字セットにも対応することができます。そのため、様々な言語のテキストを大文字化することができます。

## 詳細は次を参照

- [Swift Language Guide - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [NSHipster - String Capitalization in Swift](https://nshipster.com/string-capitalization-in-swift/)
- [Hacking with Swift - How to capitalize the first letter of a string](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)