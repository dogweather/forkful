---
date: 2024-01-20 17:42:44.919696-07:00
description: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u306E\
  \u524A\u9664\u3068\u306F\u3001\u7279\u5B9A\u306E\u898F\u5247\u306B\u57FA\u3065\u304F\
  \u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u6587\u5B57\u3092\u53D6\u308A\u9664\
  \u304F\u3053\u3068\u3067\u3059\u3002\u3053\u306E\u64CD\u4F5C\u3092\u3059\u308B\u7406\
  \u7531\u306F\u3001\u4E0D\u8981\u306A\u30C7\u30FC\u30BF\u3092\u9664\u53BB\u3057\u305F\
  \u308A\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u6574\u5F62\u3057\u305F\u308A\u3059\u308B\
  \u305F\u3081\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.069088-07:00'
model: gpt-4-1106-preview
summary: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u306E\
  \u524A\u9664\u3068\u306F\u3001\u7279\u5B9A\u306E\u898F\u5247\u306B\u57FA\u3065\u304F\
  \u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u6587\u5B57\u3092\u53D6\u308A\u9664\
  \u304F\u3053\u3068\u3067\u3059\u3002\u3053\u306E\u64CD\u4F5C\u3092\u3059\u308B\u7406\
  \u7531\u306F\u3001\u4E0D\u8981\u306A\u30C7\u30FC\u30BF\u3092\u9664\u53BB\u3057\u305F\
  \u308A\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u6574\u5F62\u3057\u305F\u308A\u3059\u308B\
  \u305F\u3081\u3067\u3059\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why?
パターンに一致する文字の削除とは、特定の規則に基づく文字列から特定の文字を取り除くことです。この操作をする理由は、不要なデータを除去したり、テキストを整形したりするためです。

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
