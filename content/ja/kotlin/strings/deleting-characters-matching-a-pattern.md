---
title:                "パターンに一致する文字を削除する"
aliases: - /ja/kotlin/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:44.919696-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/deleting-characters-matching-a-pattern.md"
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
