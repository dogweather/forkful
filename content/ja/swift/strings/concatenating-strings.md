---
aliases:
- /ja/swift/concatenating-strings/
date: 2024-01-20 17:36:19.652307-07:00
description: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u5358\u306B\u8907\
  \u6570\u306E\u6587\u5B57\u5217\u3092\u304F\u3063\u3064\u3051\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30E6\u30FC\u30B6\u30FC\
  \u306B\u8868\u793A\u3057\u305F\u308A\u30C7\u30FC\u30BF\u3092\u6574\u7406\u3057\u305F\
  \u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.222131
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u5358\u306B\u8907\
  \u6570\u306E\u6587\u5B57\u5217\u3092\u304F\u3063\u3064\u3051\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30E6\u30FC\u30B6\u30FC\
  \u306B\u8868\u793A\u3057\u305F\u308A\u30C7\u30FC\u30BF\u3092\u6574\u7406\u3057\u305F\
  \u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の連結とは、単に複数の文字列をくっつけることです。プログラマーは、ユーザーに表示したりデータを整理したりするためにこれを行います。

## How to: (やり方)
Swiftでは、`+` 演算子か文字列補間を使って文字列を連結できます。例を見てみましょう。

```Swift
let firstName = "太郎"
let lastName = "山田"
let fullName = firstName + " " + lastName  // スペースで名前を区切る
print(fullName)  // "太郎 山田" が出力される

// 文字列補間を使った例
let age = 30
let introduction = "私の名前は\(fullName)です。年齢は\(age)歳です。"
print(introduction)  // "私の名前は太郎 山田です。年齢は30歳です。" が出力される
```

## Deep Dive (掘り下げ)
歴史的に、文字列の連結方法は多岐にわたってきました。例えば、初期のプログラミング言語では、特定の関数を使うことがよくありました。しかし、Swiftでは簡潔さを重視しており、`+` 演算子や文字列補間を提供しています。

`+` 演算子を使う場合は、文字列型の変数同士を直接加えるだけですが、文字列が長くなるとパフォーマンスの観点から効率が悪くなることがあります。それに対し、文字列補間は、文字列リテラルの中に変数や定数を埋め込むことで、読みやすく、効率的なコードを書くことを可能にします。

また、文字列を合体させる際、`append()` 関数や`+=` 演算子を使う方法もあります。これらは特に、ループ処理の中で一つずつ文字列を組み立てる際に有用です。

```Swift
var message = "明日は"
message += "晴れかな？"
print(message)  // "明日は晴れかな？" が出力される
```

しかし、Swiftでは`String`が値型であるため、特に大量の文字列操作を行う際には`String`の代わりに`NSMutableString`を使用することで、パフォーマンスを向上させることができます。

## See Also (関連情報)
- Swiftの公式ドキュメント: [String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Apple Developerのガイド: [Working with Strings](https://developer.apple.com/documentation/swift/string)
