---
title:                "Kotlin: 文字列を小文字に変換する"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

こんにちは、Kotlinプログラミングの皆さん！今日の記事では、どのように文字列を小文字に変換するかについてお話ししたいと思います。文字列を小文字に変換すると何が良いのか、そしてどのように行うのかを説明します。さらに、深いレベルまでその仕組みを掘り下げていきます。

## Why

文字列を小文字に変換すると、大文字と小文字の区別が無くなって、文字列の検索や比較がより簡単になります。また、プログラムの可読性も上がるため、より効率的にコードを書くことができます。

## How To

文字列を小文字に変換する方法はいくつかありますが、今回はKotlinの組み込み関数である `toLowerCase()` を使います。以下の例をご覧ください。

```Kotlin
val str = "Hello World!"
val lowerCaseStr = str.toLowerCase()

println(str)
// 出力: Hello World!
println(lowerCaseStr)
// 出力: hello world!
```

`toLowerCase()` 関数は、指定した文字列を小文字に変換してその結果を返します。オリジナルの文字列は変更されないため、新しい変数に代入する必要があります。

また、文字列を比較する際には、両方の文字列を小文字に変換してから比較することで、大文字と小文字の区別を無くすことができます。例えば、以下のようなコードになります。

```Kotlin
val str1 = "kotlin"
val str2 = "KOTLIN"

if(str1.toLowerCase() == str2.toLowerCase()) {
    println("同じ文字列です")
} else {
    println("異なる文字列です")
}
// 出力: 同じ文字列です
```

## Deep Dive

文字列を小文字に変換する際には、言語やライブラリによって実装方法が異なることがあります。Kotlinの `toLowerCase()` 関数は、プラットフォームの基本的なルールに従って、Unicode標準を使用して小文字に変換を行います。つまり、英数字以外の文字を含む場合にも正しく変換されます。また、Turkish（トルコ語）のように、大文字と小文字が文字ごとに異なる言語にも対応しています。

さらに、文字列に `Locale` を指定することができるため、特定の言語における小文字への変換も可能です。例えば、日本語の場合は `Locale.JAPAN` を指定することで、ひらがなやカタカナも正しく小文字に変換されます。

## See Also

- [Kotlin's toLowerCase() function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Unicode Standard](https://www.unicode.org/standard/standard.html)