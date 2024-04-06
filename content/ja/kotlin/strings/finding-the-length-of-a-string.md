---
date: 2024-01-20 17:47:48.198698-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.600252-06:00'
model: gpt-4-1106-preview
summary: "How to (\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u8ABF\u3079\
  \u308B\u6A5F\u80FD\u306F\u3001\u591A\u304F\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\
  \u30B0\u8A00\u8A9E\u306E\u57FA\u672C\u7684\u306A\u90E8\u5206\u3067\u3059\u3002Kotlin\u3067\
  \u306F\u3001`String`\u30AF\u30E9\u30B9\u306B\u306F`.length`\u30D7\u30ED\u30D1\u30C6\
  \u30A3\u304C\u3042\u308A\u3001\u3053\u308C\u304C\u6587\u5B57\u5217\u306E\u9577\u3055\
  \u3092\u6570\u3048\u308B\u305F\u3081\u306B\u4F7F\u308F\u308C\u307E\u3059\u3002\u53E4\
  \u3044\u8A00\u8A9E\u3067\u306F\u3001\u632F\u308B\u821E\u3044\u304C\u7570\u306A\u308B\
  \u304B\u3082\u3057\u308C\u306A\u3044\u69D8\u3005\u306A\u95A2\u6570\u304C\u3042\u308A\
  \u307E\u3057\u305F\u3002\u4F8B\u3048\u3070\u3001C\u8A00\u8A9E\u306E`strlen()`\u306A\
  \u3069\u3067\u3059\u3002\u6587\u5B57\u5217\u304C\u7A7A\u306E\u5834\u5408\u3082\u3042\
  \u308B\u305F\u3081\u3001\u305D\u306E\u5834\u5408`.length`\u306F0\u3092\u8FD4\u3057\
  \u307E\u3059\u3002Unicode\u6587\u5B57\u3084\u7D75\u6587\u5B57\u30821\u6587\u5B57\
  \u3068\u3057\u3066\u30AB\u30A6\u30F3\u30C8\u3055\u308C\u307E\u3059\u304C\u3001\u5185\
  \u90E8\u7684\u306B\u8907\u6570\u306E\u30B3\u30FC\u30C9\u30DD\u30A4\u30F3\u30C8\u3092\
  \u542B\u3080\u3053\u3068\u304C\u3042\u308B\u306E\u3067\u6CE8\u610F\u304C\u5FC5\u8981\
  \u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
