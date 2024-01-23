---
title:                "文字列の連結"
date:                  2024-01-20T17:36:19.652307-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/concatenating-strings.md"
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
