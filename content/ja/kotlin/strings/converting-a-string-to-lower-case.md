---
date: 2024-01-20 17:38:55.613251-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u5168\u3066\u306E\u5927\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\
  \u5909\u3048\u308B\u30D7\u30ED\u30BB\u30B9\u306E\u3053\u3068\u3067\u3059\u3002\u3053\
  \u306E\u51E6\u7406\u306F\u3001\u691C\u7D22\u3084\u30BD\u30FC\u30C8\u6642\u306B\u5927\
  \u6587\u5B57\u3068\u5C0F\u6587\u5B57\u3092\u533A\u5225\u305B\u305A\u4E00\u8CAB\u3057\
  \u305F\u30C7\u30FC\u30BF\u3092\u6271\u3046\u305F\u3081\u306B\u884C\u308F\u308C\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.201436
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u5168\u3066\u306E\u5927\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\
  \u5909\u3048\u308B\u30D7\u30ED\u30BB\u30B9\u306E\u3053\u3068\u3067\u3059\u3002\u3053\
  \u306E\u51E6\u7406\u306F\u3001\u691C\u7D22\u3084\u30BD\u30FC\u30C8\u6642\u306B\u5927\
  \u6587\u5B57\u3068\u5C0F\u6587\u5B57\u3092\u533A\u5225\u305B\u305A\u4E00\u8CAB\u3057\
  \u305F\u30C7\u30FC\u30BF\u3092\u6271\u3046\u305F\u3081\u306B\u884C\u308F\u308C\u307E\
  \u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
文字列を小文字に変換するとは、全ての大文字を小文字に変えるプロセスのことです。この処理は、検索やソート時に大文字と小文字を区別せず一貫したデータを扱うために行われます。

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
