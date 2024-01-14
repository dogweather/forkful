---
title:    "Kotlin: 文字列の連結"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列をつなげることに興味があるかもしれません。それはプログラミングの世界では非常に一般的なタスクであり、データを処理する際に役立つものです。文字列をつなげることによって、大きな文字列を作成し、集約したデータを簡単に表示することができます。

## 方法

文字列をつなげる方法は多々ありますが、ここではKotlin言語のコードを使用した例を紹介します。

```Kotlin
// 最も単純な方法は+演算子を使用することです
val name = "太郎"
val greeting = "こんにちは"

println(greeting + name)
// 出力：こんにちは太郎

// .plus()メソッドを使用することもできます
val number1 = 10
val number2 = 5
println(number1.plus(number2))
// 出力：15

```

## ディープダイブ

Kotlinでは、文字列の結合には+演算子や.plus()メソッドの他にも、文字列テンプレートやStringBuilderクラスを使用することもできます。さらに、Kotlinでは文字列補間と呼ばれる機能を使用して、変数や式を文字列に埋め込むこともできます。

```Kotlin
// 文字列テンプレートを使用した例
val age = 24
val name = "太郎"

println("私の名前は$nameで、$age歳です。")
// 出力：私の名前は太郎で、24歳です。

// StringBuilderクラスを使用した例
val sb = StringBuilder()
sb.append("今日は")
sb.append("いい天気ですね。")
println(sb.toString())
// 出力：今日はいい天気ですね。

// 文字列補間を使用した例
val height = 175
val weight = 65

println("私の身長は${height}cmで、体重は${weight}kgです。")
// 出力：私の身長は175cmで、体重は65kgです。

```

## その他の参考リンク

- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/strings.html)
- [プログラミング言語Kotlinを使ってみよう](https://liginc.co.jp/421026)
- [Kotlin: 文字列結合の方法まとめ](https://qiita.com/funadara/items/4d945f1e1ff6856b639a)