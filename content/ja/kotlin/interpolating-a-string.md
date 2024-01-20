---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列補間とは、文字列中に変数値を埋め込むことです。プログラマはコードの可読性を高めるためにこれを行います。

## 使い方：

以下にKotlinでの文字列補間の基本的な使い方を示します：
```Kotlin
fun main() {
    val name = "Taro"
    println("Hello, $name")  // "Hello, Taro"
}
```
変数`name`の値が直接文字列内に埋め込まれています。式を使用したい場合は、以下のようにします：
```Kotlin
fun main() {
    val name = "Taro"
    val age = 20
    println("Hello, $name. You are ${age + 1} next year.")
    // "Hello, Taro. You are 21 next year."
}
```
age + 1の計算結果が文字列内に埋め込まれます。

## ディープダイブ

- **歴史的な視点**：文字列補間は昔から多くの言語で使われてきましたが、静的型付けシステムを持つ言語では最近になって広まりました。Kotlinでは、この機能が初めて提供されました。
- **代替案**： `+` 演算子や `format()` 関数を使う方法もありますが、文字列補間はより見やすいコードを書く方法として人気があります。
- **実装の詳細**：内部的には、Kotlinの文字列補間は `StringBuilder` と `append` を用いることで実現されています。

## 関連情報

1. [Kotlin 公式ドキュメント文字列 - 文字列テンプレート](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
2. [Kotlinプログラミング言語リファレンス - 文字列テンプレート](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)

以上が文字列補間の全貌です。上記リンクでさらに詳細な情報を見つけることができます。