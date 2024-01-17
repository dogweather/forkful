---
title:                "「文字列の長さを求める」"
html_title:           "Kotlin: 「文字列の長さを求める」"
simple_title:         "「文字列の長さを求める」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

！！！文字数を見つけるのはなに？そして、プログラマーがそれをする理由は？

## What & Why?
文字数を見つけることは、文字列内に含まれる文字の数を数えることです。プログラマーがこれをする理由は、文字列データを処理する際に必要な情報を得るためです。

## How to:
これをKotlinでやる方法を見てみましょう。

###### Example 1:
```
// 文字列の長さを出力する
val str = "Hello World"
println(str.length) // Output: 11
```

###### Example 2:
```
// 入力された文字列の長さを出力する
println("文字列を入力してください：")
val input = readLine()
println("文字列の長さは${input?.length}です。")
```

## Deep Dive:
文字列の長さを求めることで、文字列データを処理する際に必要な情報を得ることができます。以前はプログラミング言語によっては、文字列の長さを取得するために特定の関数を使用する必要がありましたが、Kotlinでは`.length`プロパティを使用することで簡単に文字列の長さを取得することができます。

## See Also:
- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [How to Get Length of String in Kotlin](https://www.baeldung.com/kotlin/string-length)