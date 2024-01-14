---
title:                "Kotlin: Patten ni matawaru moji no sakujo"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

### なぜ
なぜあるパターンに合致する文字を削除する必要があるのでしょうか？その理由を簡単に説明します。

文字列の処理において、不要な文字を削除することは非常に一般的な操作です。例えば、入力された文字列から数字のみを取り出したい場合や、特定の文字を含んでいる行を削除したい場合などに利用されます。

しかし、単純な文字列削除ではなく、あるパターンに合致する文字を削除する場合もあります。これはより複雑な処理であり、その必要性を感じることもあるでしょう。

### 方法
まずは、Kotlinで文字列の中から特定のパターンに合致する文字を削除する方法を見ていきましょう。下記のコードは、regexを使用して特定のパターンに合致する文字を置換するものです。

```Kotlin
fun main() {
    val str = "abc123def456ghi789"
    val pattern = Regex("[0-9]")
    val result = str.replace(pattern, "")
    println(result)
}
```

上記のコードを実行すると、出力結果は以下のようになります。

```
abcdefghijkl
```

このように、数字だけが置換され、文字列から削除されました。このように、Regexを使用することで、特定のパターンに合致する文字を効率的に削除することができます。

次に、文字列の中から特定の文字を削除する方法を見てみましょう。こちらはreplace関数を使用して行います。

```Kotlin
fun main() {
    val str = "I love Kotlin programming!"
    val result = str.replace(" ", "")
    println(result)
}
```

出力結果は以下のようになります。

```
IloveKotlinprogramming!
```

今度は、空白の代わりに別の文字を置換する方法を見てみましょう。次のコードは、空白を「-」に置き換えるものです。

```Kotlin
fun main() {
    val str = "I love Kotlin programming!"
    val result = str.replace(" ", "-")
    println(result)
}
```

出力結果は以下のようになります。

```
I-love-Kotlin-programming!
```

今回も、必要な文字を含めて置換されました。このように、Stringクラスのreplace関数を利用することで、文字列内の特定の文字を効率的に削除することができます。

### 深堀り
では、前述のコードで使用したRegexやreplace関数の詳細について深く掘り下げてみましょう。

まず、Regexクラスは、正規表現を扱うことができるクラスです。正規表現とは、特定のパターンを表現するための文字列のことです。例えば、[0-9]という正規表現は0から9までの数字のいずれかに合致することを意味します。

Regexクラスを使用することで、特定のパターンに合致する文字を見つけることができます。そして、replace関数を使用することで、見つかった文字を別の文字で置き換えることができます。

また、replace関数は利用する際にRegExpパラメータを指定することができます。これにより、置き換える文字をより細かく指定するこ