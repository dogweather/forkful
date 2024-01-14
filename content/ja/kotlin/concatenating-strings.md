---
title:                "Kotlin: 文字列の結合"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
文字列を連結することに関心を持つのはなぜでしょうか。文字列の連結は、プログラミングにおいて非常に重要な機能の一つです。コードを効率的に書くためには、文字列を連結してデータを処理する必要があります。

## 方法
Kotlinでは、文字列を連結するために「+」演算子を使用します。以下は、簡単なコード例です。

```Kotlin
val str1 = "Hello"
val str2 = "World"
val result = str1 + " " + str2
println(result)
```

上記のコードを実行すると、次のような出力が得られます。

```
Hello World
```

ここでは、2つの変数「str1」および「str2」に値を割り当て、それらの値を「+」演算子を使用して連結し、新しい変数「result」に割り当てています。そして、「println」関数を使用してresultの値を出力しています。

さらに、Kotlinでは「StringBuilder」クラスを使用して複数の文字列を連結することもできます。以下は、その例です。

```Kotlin
val sb = StringBuilder()
sb.append("Hello")
sb.append(" ")
sb.append("World")
val result = sb.toString()
println(result)
```

これにより、同じ結果が得られますが、より効率的に文字列を連結できます。

## ディープダイブ
文字列の連結に関して、さらに詳しく見てみましょう。文字列を連結することで、複数の文字列を結合した新しい文字列を作成できます。また、文字列以外の値を文字列に変換してから連結することもできます。

文字列の連結では、「+」演算子を使用することが最も一般的ですが、文字列を大量に連結する場合は、StringBuilderクラスを使用することでより効率的に処理することができます。

## その他の記事
「時短テクニック：Kotlinの文字列連結方法」  
https://www.howkaku.com/kotlin/string-concatenation

「Kotlinプログラミングの基本：文字列の連結」  
https://dev.classmethod.jp/articles/kotlin-basic-type-string/

「文字列の連結の効率性を向上させる方法」  
https://proandroiddev.com/make-efficiency-strategies-of-java-concatenation-in-kotlin-world-cb262820a2d5

## 参考リンク
「Kotlin Strings」  
https://kotlinlang.org/docs/reference/basic-types.html#string

「Java StringBuilderクラス」  
https://docs.oracle.com/javase/jp/1.5.0/docs/api/java/lang/StringBuilder.html