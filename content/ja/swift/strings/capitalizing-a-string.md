---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:35.668817-07:00
description: "Swift\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\
  \u3068\u306F\u3001\u4E0E\u3048\u3089\u308C\u305F\u6587\u5B57\u5217\u306E\u6700\u521D\
  \u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u3057\u3001\u6B8B\u308A\u306E\u6587\
  \u5B57\u3092\u5C0F\u6587\u5B57\u306B\u5909\u66F4\u3059\u308B\u3053\u3068\u3092\u6307\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u6587\u6CD5\
  \u30EB\u30FC\u30EB\u3084\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\
  \u30FC\u30B9\u57FA\u6E96\u306B\u5F93\u3063\u3066\u540D\u524D\u3084\u6587\u3092\u6574\
  \u5F62\u3059\u308B\u76EE\u7684\u306A\u3069\u306E\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.588497-06:00'
model: gpt-4-0125-preview
summary: "Swift\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3068\
  \u306F\u3001\u4E0E\u3048\u3089\u308C\u305F\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\
  \u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u3057\u3001\u6B8B\u308A\u306E\u6587\u5B57\
  \u3092\u5C0F\u6587\u5B57\u306B\u5909\u66F4\u3059\u308B\u3053\u3068\u3092\u6307\u3057\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u6587\u6CD5\u30EB\
  \u30FC\u30EB\u3084\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\
  \u30B9\u57FA\u6E96\u306B\u5F93\u3063\u3066\u540D\u524D\u3084\u6587\u3092\u6574\u5F62\
  \u3059\u308B\u76EE\u7684\u306A\u3069\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 何となぜ？

Swiftで文字列を大文字化するとは、与えられた文字列の最初の文字を大文字にし、残りの文字を小文字に変更することを指します。プログラマーは、文法ルールやユーザーインターフェース基準に従って名前や文を整形する目的などのためにこれを行います。

## 方法：

Swiftの`String`構造体には、文字列の大文字と小文字を操作するための組み込みメソッドがいくつかあります。ここでは、Swiftで文字列を大文字化する方法について、標準メソッドの使用と必要に応じてサードパーティーライブラリの使用を含むいくつかのアプローチを紹介します。

### 組み込みメソッドを使用

文字列の最初の文字を大文字にし、残りを小文字にするには：

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // 出力: "Hello, world"
```

文中の各単語の最初の文字を大文字にするには、`capitalized`プロパティを使用できます：

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // 出力: "Hello, World"
```

### サードパーティーライブラリを使用

Swiftの標準ライブラリは非常に包括的ですが、特定の大文字化フォーマットにはより複雑な操作が必要になる場合や、サードパーティーライブラリを使用することで単純化できる場合があります。文字列操作に人気のあるライブラリの一つにSwiftRichStringがあります。（注：サードパーティーライブラリをSwift Package Manager、CocoaPods、またはCarthageを通して追加し、ファイルにインポートすることを常に確認してください。）

まず、プロジェクトに`SwiftRichString`を追加する必要があります。インストールされたら、特定の大文字化の必要性を含むさまざまな文字列操作を行うことができます。しかし、現在のところ、Swiftの組み込みメソッドだけで文字列を大文字化するほとんどのユースケースを十分にカバーしており、外部ライブラリを使う必要はほとんどありません。

ライブラリの最新のドキュメンテーションを常に参照し、メソッドの更新や変更について確認してください。
