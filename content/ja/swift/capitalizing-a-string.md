---
title:                "文字列を大文字化する"
html_title:           "Swift: 文字列を大文字化する"
simple_title:         "文字列を大文字化する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列の大文字化とは文字列内のすべての単語の最初の文字を大文字にすることを言います。プログラマーたちはコードを整理したり、ユーザーフレンドリな表示を作るためによくこれを使用します。

## どうやって:

Swiftでは、 `uppercased()` メソッドを使用して文字列を簡単に大文字化できます。以下にその方法を示します:

```Swift
let lowerCaseString = "hello world"
let upperCaseString = lowerCaseString.uppercased()
print(upperCaseString)
```

出力:
```
HELLO WORLD
```

## ディープダイブ

文字列を大文字化するこの概念は、コンピューターが開発されて以来ずっと存在しています。早い時期には、大文字と小文字の区別がありませんでしたが、やがて異なるケースが導入され、この操作が重要になりました。

代替手段として、各単語の最初だけ大文字にする `capitalized` メソッドもあります。

```Swift
let lowerCaseString = "hello world"
let capitalizedString = lowerCaseString.capitalized
print(capitalizedString)
```

出力:
```
Hello World
```

Swiftでは、 `uppercased()` と `capitalized` は共に `String` タイプのインスタンスメソッドです。これらは元の文字列を変更せず、新しい大文字化された文字列を返します。

## 参照

1. [Apple 文字列と文字 - Swift プログラミングガイド](https://developer.apple.com/jp/documentation/swift/String_and_Character)
2. [スタック・オーバーフロー - Swiftで文字列を大文字にする](https://stackoverflow.com/questions/24159386/swift-capitalize-words-in-string)