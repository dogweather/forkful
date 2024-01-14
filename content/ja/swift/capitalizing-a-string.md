---
title:                "Swift: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列の大文字に変換することに興味があるかもしれません。文字列の大文字と小文字を切り替えることは、テキストデータをより表現豊かにする方法の一つです。Swiftでは、文字列を簡単に大文字に変換する方法がたくさんありますので、これから紹介します。

## 方法
```Swift
var text = "hello, world!"
print(text.uppercased())
```

このコードを実行すると、`HELLO, WORLD!`という文字列が出力されます。`uppercased()`は、文字列のメソッドの一つで、文字列を大文字に変換します。また、`capitalized`メソッドを使えば、最初の文字だけが大文字になるように変換することもできます。

```Swift
var text = "hello, world!"
print(text.capitalized)
```

上記のコードを実行すると、`Hello, world!`という文字列が出力されます。

## 深堀り
これらのメソッドを使うと、テキストデータをより表現豊かに、または特定のフォーマットに合わせて変換できます。さらに、カスタムの大文字変換メソッドを作成することもできます。たとえば、特定の言語において、文の先頭にくる名詞だけを大文字にするような変換を作成することができます。文字列操作における大文字変換は、テキストデータを柔軟に扱うための重要な手段です。

### 後処理
文字列を大文字に変換することは、単純であるように思えますが、実はいくつかの注意点があります。半角の`i`や`j`のように、大文字と小文字が似ている文字を使う際は、大文字に変換されても形が崩れないように注意しましょう。また、言語によっては、大文字に変換すると意味が変わってしまう場合もありますので、その点にも注意してください。

## 参考
- [公式ドキュメント](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Strings and Characters - Apple Developer](https://developer.apple.com/documentation/swift/strings_and_characters)
- [How to convert a string to uppercase Letters in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-convert-a-string-to-uppercase-letters-in-swift)

---
## 参考リンク