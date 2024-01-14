---
title:                "Kotlin: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

# なぜ文字列を連結するのか

文字列を連結することは、アプリケーションやプログラムを開発する際に非常に重要な操作です。文字列を結合することにより、複数の情報を1つの文字列としてまとめることができます。これにより、情報を効率的に扱うことができます。

## 連結する方法

Kotlinでは、文字列を結合するために+演算子を使用します。以下の例では、2つの文字列を結合して出力する方法を示します。

```Kotlin
val name = "田中"
val greeting = "こんにちは"
println(name + greeting)
```

出力結果は「田中こんにちは」となります。

Kotlinでは、さまざまなデータ型を文字列に変換する「toString()」メソッドも使用することができます。例えば、数値を文字列として結合することも可能です。

```Kotlin
val age = 30
val message = "私は" + age.toString() + "歳です"
println(message)
```

出力結果は「私は30歳です」となります。

## 深堀り

Kotlinでは文字列の結合についてもっと深く理解することができます。さまざまな方法で文字列を結合することができるため、開発者にとって非常に柔軟な言語です。

例えば、Kotlinでは文字列テンプレートを使用することで、文字列の中に変数を埋め込むことができます。文字列テンプレートを使うと、文字列を結合する際に + 演算子を使用する必要がなくなり、コードがより簡潔になります。

また、文字列リテラル内で文字列の結合も可能です。以下のように、$を使用して変数を直接文字列に埋め込むことができます。

```Kotlin
val food = "ラーメン"
println("私は今日、$foodを食べました")
```

出力結果は「私は今日、ラーメンを食べました」となります。

## 参考リンク

- [Kotlin文字列結合方法](https://qiita.com/ngsw_taro/items/7092cda7e30f7be91a71)
- [Kotlin文字列結合の仕組み](https://www.atmarkit.co.jp/ait/articles/2006/17/news026.html)
- [Kotlin公式ドキュメント - 文字列とテンプレート](https://kotlinlang.org/docs/basic-types.html#strings)

# 関連記事

- [Kotlinで文字列を分割する方法](https://www.linktocoding.com/posts/223)
- [Kotlinの基礎 - 文字列操作について学ぶ](https://www.guru99.com/strings-stringbuilder-stringbuffer-in-kotlin.html)