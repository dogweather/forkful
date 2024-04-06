---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:35.668817-07:00
description: "\u65B9\u6CD5\uFF1A Swift\u306E`String`\u69CB\u9020\u4F53\u306B\u306F\
  \u3001\u6587\u5B57\u5217\u306E\u5927\u6587\u5B57\u3068\u5C0F\u6587\u5B57\u3092\u64CD\
  \u4F5C\u3059\u308B\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30E1\u30BD\u30C3\u30C9\
  \u304C\u3044\u304F\u3064\u304B\u3042\u308A\u307E\u3059\u3002\u3053\u3053\u3067\u306F\
  \u3001Swift\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u65B9\
  \u6CD5\u306B\u3064\u3044\u3066\u3001\u6A19\u6E96\u30E1\u30BD\u30C3\u30C9\u306E\u4F7F\
  \u7528\u3068\u5FC5\u8981\u306B\u5FDC\u3058\u3066\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\
  \u30A3\u30FC\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u4F7F\u7528\u3092\u542B\u3080\u3044\
  \u304F\u3064\u304B\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\u3092\u7D39\u4ECB\u3057\u307E\
  \u3059\u3002"
lastmod: '2024-04-05T22:38:42.092956-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Swift\u306E`String`\u69CB\u9020\u4F53\u306B\u306F\u3001\
  \u6587\u5B57\u5217\u306E\u5927\u6587\u5B57\u3068\u5C0F\u6587\u5B57\u3092\u64CD\u4F5C\
  \u3059\u308B\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30E1\u30BD\u30C3\u30C9\u304C\
  \u3044\u304F\u3064\u304B\u3042\u308A\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\
  Swift\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u65B9\u6CD5\
  \u306B\u3064\u3044\u3066\u3001\u6A19\u6E96\u30E1\u30BD\u30C3\u30C9\u306E\u4F7F\u7528\
  \u3068\u5FC5\u8981\u306B\u5FDC\u3058\u3066\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\
  \u30FC\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u4F7F\u7528\u3092\u542B\u3080\u3044\u304F\
  \u3064\u304B\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\u3092\u7D39\u4ECB\u3057\u307E\u3059\
  \u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

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
