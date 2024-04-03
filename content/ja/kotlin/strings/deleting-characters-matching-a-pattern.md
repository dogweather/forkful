---
date: 2024-01-20 17:42:44.919696-07:00
description: 'How to: .'
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.042963-06:00'
model: gpt-4-1106-preview
summary: .
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
