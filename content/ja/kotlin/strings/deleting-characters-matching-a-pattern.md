---
date: 2024-01-20 17:42:44.919696-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.357487-06:00'
model: gpt-4-1106-preview
summary: "Kotlin\u3067\u306F\u3001\u6587\u5B57\u5217\u304B\u3089\u30D1\u30BF\u30FC\
  \u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u306B\
  \u306F\u4E3B\u306B`replace()`\u95A2\u6570\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\
  \u3053\u308C\u306F\u6B63\u898F\u8868\u73FE\u3092\u5229\u7528\u3057\u3066\u884C\u308F\
  \u308C\u3001Java\u306E`String`\u30AF\u30E9\u30B9\u306B\u7531\u6765\u3057\u307E\u3059\
  \u3002\u5BFE\u5FDC\u3059\u308B\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u7528\u3059\u308B\
  \u3053\u3068\u3067\u3001\u975E\u5E38\u306B\u5F37\u529B\u306A\u30D1\u30BF\u30FC\u30F3\
  \u30DE\u30C3\u30C1\u30F3\u30B0\u3068\u7F6E\u63DB\u304C\u53EF\u80FD\u3067\u3059\u3002\
  `filter`\u95A2\u6570\u3092\u4F7F\u7528\u3059\u308B\u3068\u3001\u7279\u5B9A\u306E\
  \u6761\u4EF6\u306B\u30DE\u30C3\u30C1\u3059\u308B\u6587\u5B57\u3060\u3051\u3092\u6B8B\
  \u3059\u3053\u3068\u3082\u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F`replace()`\u3088\
  \u308A\u30B7\u30F3\u30D7\u30EB\u306A\u30B1\u30FC\u30B9\u3084\u6761\u4EF6\u306B\u57FA\
  \u3065\u304F\u30D5\u30A3\u30EB\u30BF\u30EA\u30F3\u30B0\u306B\u4FBF\u5229\u3067\u3059\
  ."
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

## How to:
```kotlin
fun main() {
    val regex = Regex("[aeiou]")
    val input = "こんにちは, 世界!"
    val output = input.replace(regex, "")
    println(output) // "こんにちは, 世界!"
}

fun main() {
    val input = "電話番号は080-1234-5678です。"
    val digitsOnly = input.filter { it.isDigit() }
    println(digitsOnly) // "08012345678"
}
```

## Deep Dive
Kotlinでは、文字列からパターンに一致する文字を削除するには主に`replace()`関数を使用します。これは正規表現を利用して行われ、Javaの`String`クラスに由来します。対応する正規表現を使用することで、非常に強力なパターンマッチングと置換が可能です。`filter`関数を使用すると、特定の条件にマッチする文字だけを残すこともできます。これは`replace()`よりシンプルなケースや条件に基づくフィルタリングに便利です。

古くはPerlやsedなどのツールで強力なテキスト処理機能が提供されていましたが、Kotlinではこれらの機能をネイティブにサポートし、さらに型安全な方法で利用できます。`replace()`の利用はシンプルだけど、その背後にはJavaにおける正規表現の強力なフレームワークがあります。Kotlinの標準ライブラリはこの機能を簡潔かつ読みやすく提供してくれます。

## See Also
- Kotlinの正規表現について深く学ぶ: [Kotlin Regex documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Javaの`Pattern`クラスについて学ぶ: [Pattern (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
