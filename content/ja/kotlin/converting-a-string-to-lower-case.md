---
title:    "Kotlin: 小文字に文字列を変換する"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## なぜ

 文字列を小文字に変換するのに、なぜ参加するかの理由は単純です。プログラミングにおいて、文字列を比較する必要がある場合、大文字と小文字の違いが原因でエラーが発生することがあります。そのため、文字列をすべて小文字に変換することで、比較や処理をより正確に行うことができます。

## 方法

まずは、Kotlinの文字列を小文字に変換するメソッドである`toLowerCase()`を使用します。このメソッドは、変換後の文字列を返すだけで元の文字列は変更されません。例えば、次のコードをKotlin Playgroundなどで実行すると、

```
Kotlin
fun main() {
    val sampleString = "HeLLo wOrLd"
    val lowerCaseString = sampleString.toLowerCase()
    println(lowerCaseString)
}
```

出力結果は`hello world`となります。また、`toLowerCase()`の代わりに、`replace()`メソッドを使用して正規表現を使い、大文字の文字を小文字に置き換える方法もあります。

```
Kotlin
fun main() {
    val sampleString = "HeLLo wOrLd"
    val regularExpression = Regex("[A-Z]")
    val lowerCaseString = sampleString.replace(regularExpression) { it.value.toLowerCase() }
    println(lowerCaseString)
}
```

出力結果は先ほどと同じく`hello world`となります。ただし、こちらは元の文字列を変更してしまうので注意が必要です。

## ディープダイブ

実は、文字列を小文字に変換するには文字コードの変換が行われています。Kotlinでは、文字列をUnicodeと呼ばれる16ビット文字コードで表現しています。小文字のUnicodeコードは、大文字のUnicodeコードに比べて`+32`という偶数の差があります。つまり、小文字に変換することはUnicodeコードを`+32`することになります。しかし、この方法だと例外的な文字や言語によっては正しく変換されない場合があります。そのため、KotlinではASCII文字のみを変換するよう最適化されています。

## 関連リンク

- [Kotlin Playground](https://play.kotlinlang.org)
- [Kotlinの文字列操作](https://kotlinlang.org/docs/reference/strings.html)
- [JavaとKotlinの文字列の違い](https://www.quora.com/What-are-the-major-differences-between-Java-strings-and-Kotlin-strings)
- [Unicodeとは？](https://unicode.org/standard/WhatIsUnicode.html)