---
title:                "文字列の長さを求める"
aliases:
- ja/kotlin/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:48.198698-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
文字列の長さを測るってのは、その文字列に含まれる文字の数を数えることです。プログラマは、入力検証、データ処理、UIの設計などのためにこれを行います。

## How to (方法)
```kotlin
fun main() {
    val greeting = "こんにちは"
    val length = greeting.length

    println("The length of the string is: $length")
    // 出力: The length of the string is: 5
}
```

## Deep Dive (掘り下げ)
文字列の長さを調べる機能は、多くのプログラミング言語の基本的な部分です。Kotlinでは、`String`クラスには`.length`プロパティがあり、これが文字列の長さを数えるために使われます。古い言語では、振る舞いが異なるかもしれない様々な関数がありました。例えば、C言語の`strlen()`などです。文字列が空の場合もあるため、その場合`.length`は0を返します。Unicode文字や絵文字も1文字としてカウントされますが、内部的に複数のコードポイントを含むことがあるので注意が必要です。

## See Also (関連情報)
- Kotlin公式ドキュメントの[String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)クラス 
- [Unicodeとは？](https://unicode.org/standard/WhatIsUnicode.html)
- [Kotlinでの文字列の操作](https://kotlinlang.org/docs/strings.html#string-literals)
