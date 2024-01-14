---
title:                "Kotlin: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ？

Javaでプログラミングをしているあなたに、Kotlinを使ったことはありますか？KotlinはJavaよりもシンプルで、保守しやすく、より簡単に学ぶことができるプログラミング言語です。今回は、Kotlinを使ってコードで部分文字列を抽出する方法を紹介します。

## 抽出する方法

Kotlinで部分文字列を抽出するには、```substring()```メソッドを使用します。このメソッドには、2つのパラメーターが必要です。1つ目のパラメーターには、部分文字列の開始位置を示すインデックスを指定し、2つ目のパラメーターには、部分文字列の終了位置を示すインデックスを指定します。例えば、次のようにコードを書くことで、文字列の3文字目から5文字目までの部分文字列を抽出することができます。

```Kotlin
val fullString = "こんにちは、世界！"
val substring = fullString.substring(2, 5)
println(substring)
```

出力結果は「んにちは」となります。

また、2つのパラメーターの代わりに、部分文字列の始まりの位置のみを指定することもできます。この場合、始まりの位置から文字列の最後までを抽出します。

```Kotlin
val fullString = "こんにちは、世界！"
val substring = fullString.substring(4)
println(substring)
```

出力結果は「ちは、世界！」となります。

## ディープダイブ

実際には、```substring()```メソッドは、```substringAfter()```や```substringBefore()```などの関連メソッドと組み合わせて使うことができます。これらのメソッドは、文字列内の特定の文字の後または前にある部分文字列を抽出するために使用されます。また、```indexOf()```メソッドを使用することで、文字列内での特定の文字のインデックスを調べることができます。

このように、Kotlinには多様な文字列操作の方法が用意されているので、データ処理やテキスト処理の際に非常に便利です。

## See Also

Kotlinにおける文字列操作の詳細については、公式ドキュメントを参照してください。
- https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/index.html
- https://www.geeksforgeeks.org/substring-method-in-kotlin/
- https://www.baeldung.com/substring-kotlin