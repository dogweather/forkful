---
title:                "「文字列の連結」"
html_title:           "Kotlin: 「文字列の連結」"
simple_title:         "「文字列の連結」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の連結を行うことの利点は、文字列をより効率的に処理することができるためです。例えば、各文字列を個別に操作するのではなく、1つの長い文字列として処理することで、より複雑な操作を簡単に行うことができます。

## 方法

文字列の連結を行うには、Kotlin言語で用意されている`+`演算子を使用します。例えば、以下のように書くことで複数の文字列を連結することができます。

```Kotlin
val str1 = "Hello"
val str2 = "world"
val str3 = str1 + ", " + str2
print(str3) // 出力結果: Hello, world
```

また、文字列の連結には`plus()`関数も利用することができます。

```Kotlin
val str1 = "Hello"
val str2 = "world"
val str3 = str1.plus(", ").plus(str2)
print(str3) // 出力結果: Hello, world
```

さらにKotlinでは、文字列テンプレートという機能を使用することで、連結ではなく文字列の中に変数を埋め込むこともできます。

```Kotlin
val name = "John"
val str = "Hello, $name"
print(str) // 出力結果: Hello, John
```

## ディープダイブ

文字列の連結は内部的にはStringBuilderクラスを使用して行われます。StringBuilderは可変の文字列を扱うためのクラスであり、`append()`メソッドを使用することで文字列を連結していきます。

また、文字列の連結は単純な操作のように見えますが、大きなループ内で行われる場合などにはパフォーマンス上の問題が生じる可能性があります。そのため、より高速な文字列連結を行えるように最適化された`StringBuilder`クラスを使用することを推奨します。

## その他

- [Kotlinプログラミング言語公式サイト](https://kotlinlang.org/)：Kotlinの最新情報やリファレンス、ドキュメンテーションなどが掲載されています。
- [Kotlinコミュニティフォーラム](https://discuss.kotlinlang.org/)：多くのKotlinユーザーが参加し、質問や議論を行うことができるコミュニティフォーラムです。