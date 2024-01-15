---
title:                "文字列の先頭を大文字にする"
html_title:           "Swift: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
今回は、Swiftで文字列の大文字化を行う方法について紹介します。文字列の大文字化を行うことで、テキストの表示を統一したり、処理を使いやすくしたりすることができます。

## How To
文字列を大文字化するには、`uppercased()`メソッドを使用します。例として、以下のようなコードを書きます。

```Swift
let sentence = "this is a sample sentence"
let uppercasedSentence = sentence.uppercased()

print(uppercasedSentence)
```

このコードを実行すると、`THIS IS A SAMPLE SENTENCE`という出力が得られます。

## Deep Dive
`uppercased()`メソッドは、文字列をすべて大文字に変換するだけでなく、特定のロケールに合わせて変換することもできます。デフォルトでは、システムの規定のロケールが使用されますが、`uppercased(with:)`メソッドを使用することで任意のロケールを指定することができます。

また、文字列以外のデータ型でも大文字化することができます。例えば、`Int`型の変数を`String`型に変換し、`uppercased()`メソッドを使用することで、数値を大文字の文字列に変換することができます。

## See Also
参考リンク：
- [Official documentation for `uppercased()`](https://developer.apple.com/documentation/swift/string/3127166-uppercased)
- [Tutorial for string manipulation in Swift](https://www.raywenderlich.com/5527-string-cheat-sheet-for-swift-4-and-later)

コード例や詳細な解説は、上記のリンクを参考にしてください。