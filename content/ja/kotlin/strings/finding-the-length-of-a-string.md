---
date: 2024-01-20 17:47:48.198698-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6E2C\u308B\u3063\u3066\u306E\
  \u306F\u3001\u305D\u306E\u6587\u5B57\u5217\u306B\u542B\u307E\u308C\u308B\u6587\u5B57\
  \u306E\u6570\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u3001\u5165\u529B\u691C\u8A3C\u3001\u30C7\u30FC\u30BF\u51E6\u7406\
  \u3001UI\u306E\u8A2D\u8A08\u306A\u3069\u306E\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.205664
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6E2C\u308B\u3063\u3066\u306E\
  \u306F\u3001\u305D\u306E\u6587\u5B57\u5217\u306B\u542B\u307E\u308C\u308B\u6587\u5B57\
  \u306E\u6570\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u3001\u5165\u529B\u691C\u8A3C\u3001\u30C7\u30FC\u30BF\u51E6\u7406\
  \u3001UI\u306E\u8A2D\u8A08\u306A\u3069\u306E\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
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
