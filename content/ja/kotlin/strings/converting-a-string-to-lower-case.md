---
date: 2024-01-20 17:38:55.613251-07:00
description: "How to: (\u65B9\u6CD5) Kotlin\u3067\u6587\u5B57\u5217\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u3048\u308B\u306E\u306F\u7C21\u5358\u3067\u3059\u3002\u6A19\u6E96\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u306E`toLowerCase()`\u95A2\u6570\u3092\u4F7F\u3044\
  \u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.933914-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Kotlin\u3067\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\
  \u5909\u3048\u308B\u306E\u306F\u7C21\u5358\u3067\u3059\u3002\u6A19\u6E96\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306E`toLowerCase()`\u95A2\u6570\u3092\u4F7F\u3044\u307E\u3057\
  \u3087\u3046\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

## How to: (方法)
Kotlinで文字列を小文字に変えるのは簡単です。標準ライブラリの`toLowerCase()`関数を使いましょう。

```kotlin
fun main() {
    val originalString = "Kotlin Programming LANGUAGE"
    val lowerCasedString = originalString.toLowerCase()
    println(lowerCasedString)
}
```

出力は次の通りです:

```
kotlin programming language
```

## Deep Dive (深掘り)
`toLowerCase()`関数は国際化の一部として登場しました。Unicode標準を取り入れることで、さまざまな言語と文化圏の文字にも対応しています。もしロケール特有の大小文字変換が必要な場合、`toLowerCase(Locale)`を使用すればいいでしょう。

また、`toLowerCase()`の代替として、`decapitalize()`がありますが、これは最初の文字だけを小文字に変えるものです。実装の観点から言うと、`toLowerCase()`は内部で`String`の各文字に対して小文字化を行うため、文字列が長いとその分処理時間がかかります。パフォーマンスを気にする場合は、必要に応じてその処理を最適化することを考慮しましょう。

## See Also (関連情報)
- Kotlinの公式ドキュメンテーション [`toLowerCase()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- Unicode標準についての詳細 [Unicode](https://www.unicode.org)
- Java `String`クラスのドキュメンテーション [`toLowerCase(Locale)`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#toLowerCase(java.util.Locale))
