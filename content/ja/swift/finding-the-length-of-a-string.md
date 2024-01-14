---
title:                "Swift: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを取得することに取り組む理由は何でしょうか？簡単に1-2文で説明します。

文字列を操作する必要がある場合、その文字列の長さを知ることは非常に重要です。例えば、ユーザーが入力した文字列が指定した長さよりも長い場合、その文字列を切り取る必要があるかもしれません。そのため、文字列の長さを取得できることは非常に便利な機能です。

## ハウツー

文字列の長さを取得する方法を実際のコード例とともに説明します。

```Swift
let str = "こんにちは！こんにちは！"
print(str.count)
// 出力：15
```

上記のコードでは、`count`を使用して文字列の長さを取得しています。このプロパティは、文字列に含まれる文字の数をカウントするために使用されます。

## ディープダイブ

より詳細な情報をお届けします。文字列の長さを取得する方法は、文字列の種類によって異なることがあります。例えば、日本語や絵文字のようなマルチバイト文字を含む文字列をカウントする場合、結果が異なる可能性があります。そのため、文字列の長さを取得する際は、文字のエンコーディングなども考慮する必要があります。

また、Swiftでは、文字列の長さを取得するために`count`の代わりに`lengthOfBytes(using:)`メソッドを使用することもできます。しかし、このメソッドはSwift 5.0以前の古いバージョンでのみ使用可能です。

## もっと詳しく学ぶ

- [Swift公式ドキュメント - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [iOSアプリ開発の基礎：Swiftで文字列を操作する方法](https://dev.classmethod.jp/articles/swift-string-operation/)

## 関連リンク

- [Markdown 101 - Basic Syntax](https://www.markdownguide.org/basic-syntax/)
- [Learn the Basics of Swift in 10 Minutes](https://learnappmaking.com/learn-swift-for-beginners/)