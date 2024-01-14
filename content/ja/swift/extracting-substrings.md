---
title:    "Swift: 字句の抽出"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列からサブストリングを抽出することに関心を持つのはなぜでしょうか？実際には、アプリケーションのユーザーが入力した文字列から特定の部分を取得したい場合があります。例えば、メールアドレスを入力させるフォームがあるとします。その場合、入力された文字列からドメインを抽出したいと考えるかもしれません。サブストリングを抽出することで、特定の部分を簡単に取得することができます。

## 技術的手順

サブストリングを抽出するための基本的な手順を紹介します。まず、抽出したい文字列を取得します。次に、`substring`メソッドを使用し、開始位置と終了位置を指定してサブストリングを抽出します。最後に、抽出したサブストリングを使って任意の処理を行います。

```Swift
// サブストリングを抽出するための例
let text = "Hello Swift"
let start = text.index(text.startIndex, offsetBy: 6)
let end = text.index(text.endIndex, offsetBy: -6)
let result = text.substring(with: start..<end) // "Swift"
```

## 深堀り

サブストリングを抽出する際に注意すべき点があります。それは、文字列のインデックスの扱いです。`startIndex`は常に文字列の先頭を指し、`endIndex`は常に文字列の末尾の1つ後ろを指します。しかし、`index`メソッドを使用する場合は注意が必要です。`index`メソッドの第2引数に指定するオフセットは、文字の数ではなくインデックスの数を表します。つまり、`Hello Swift`という文字列の最後の文字を指定するためには、`6`ではなく`5`(文字列の先頭から数えて6番目の文字は`S`ではなく`f`)を指定する必要があります。

## さらに参考にする

この記事で紹介したサブストリングの抽出方法は、Swiftの基本的な機能に限定したものです。Swiftには多数の文字列操作に関する高度なメソッドやプロパティが用意されています。詳しくは以下のリンクを参考にしてください。

- [Swift Strings and Characters - Apple Developer Documentation](https://developer.apple.com/documentation/swift/strings_and_characters)
- [Working with Strings in Swift 4 - Hacking with Swift](https://www.hackingwithswift.com/example-code/strings/working-with-strings-in-swift)