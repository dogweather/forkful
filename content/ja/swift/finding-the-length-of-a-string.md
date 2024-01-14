---
title:                "Swift: 文字列の長さを見つける"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることに取り組む理由は、プログラムで文字列を正しく処理するために重要です。

## 方法

文字列の長さを求める方法は、Swiftの`count`メソッドを使用することです。例えば、次のように書きます。

```Swift
let str = "こんにちは！"
print(str.count)
```

このコードを実行すると、出力結果は`6`となります。何も考えずに単純に文字列の長さを数える方法とは異なり、`count`メソッドはUnicodeの規格に基づいて文字列の長さを求めます。

文字列が空白である場合でも、`count`メソッドは`0`を返します。また、絵文字や結合文字など、一つの文字でも複数のコードポイントを持つ特殊な文字を含む場合でも、`count`メソッドは正しく文字列の長さを求めます。

## ディープダイブ

`count`メソッドは実際には`String`構造体が持つプロパティの一つであり、内部的には文字列をUnicodeの配列として扱います。そのため、文字列の長さを求める際には様々な注意点があります。

例えば、文字列の長さを求める前に文字列のエンコーディングについて調べる必要があります。`count`メソッドはUnicodeの配列を数えるため、UTF-8のようなマルチバイト文字を含むエンコーディングでは、文字列の長さを正確に求めることができません。

また、`count`メソッドは経験的にはO(n)の時間を要しますが、大きな文字列である場合にはnが増えるため計算量も増加していきます。そのため、文字列の長さを頻繁に求める処理を行う場合には、効率的なデータ構造を選択するなどの工夫が必要です。

## 参考

- [Swift Documentation: String.count](https://developer.apple.com/documentation/swift/string/2961464-count)
- [SwiftDoc: Unicode](https://swiftdoc.org/v3.0/type/Unicode/)
- [Hacking with Swift: How to count characters in a string using Unicode](https://www.hackingwithswift.com/example-code/strings/how-to-count-characters-in-a-string-using-unicode/)
- [Unicode Technical Report 29: Text Boundaries](https://www.unicode.org/reports/tr29/)