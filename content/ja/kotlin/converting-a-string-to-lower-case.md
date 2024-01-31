---
title:                "文字列を小文字に変換"
date:                  2024-01-20T17:38:55.613251-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-string-to-lower-case.md"
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
