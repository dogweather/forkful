---
title:                "部分文字列の抽出"
html_title:           "Kotlin: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何かとは？
サブストリングを抽出するとは、文字列の箇所を切り出すことを指します。プログラマーがこれを行う理由は、特定の文字列の一部を必要とする場合や、文字列の内容を比較する際に役立つためです。

## 方法：
サブストリングを抽出する最も基本的な方法は、文字列の範囲を指定して `substring()` メソッドを使用することです。例えば、次のコードは、`Hello world`という文字列から`world`という部分を抽出します。

```Kotlin
val str = "Hello world"
val sub = str.substring(6, 11)
println(sub) // output: world
```

他にも、正規表現を使用してパターンに一致する文字列を抽出することもできます。例えば、次のコードは、文字列から数字のみを抽出します。

```Kotlin
val str = "My age is 25"
val sub = str.replace(Regex("[^0-9]"), "")
println(sub) // output: 25
```

## より詳しく：
サブストリングの抽出には、多くの方法がありますが、それらはすべて同じ目的を持つものです。サブストリングの抽出は、多くのプログラミング言語でサポートされており、Kotlinでも例外ではありません。正規表現を使用することで、より複雑な抽出を行うこともできます。ただし、正規表現の処理にはノウハウが必要であり、パフォーマンスにも影響することがあります。そのため、単純なサブストリング抽出には、基本的なメソッドを使用することをお勧めします。

## 関連情報：
もし、さらに詳しくサブストリング抽出について学びたい場合は、以下のリンクを参考にしてください。

- Kotlin 公式ドキュメント: https://kotlinlang.org/docs/strings.html#string-slices
- JavaDoc: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-
- 正規表現について学べるサイト: https://regexone.com/