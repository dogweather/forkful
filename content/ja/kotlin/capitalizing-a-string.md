---
title:                "文字列の先頭を大文字にする"
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の先頭を大文字にするって？ 文字列の先頭を大文字に変更することです。読みやすさ向上、データの整形、または文書の表記規則に従うためにプログラマーはこれを行います。

## How to: (方法)
```kotlin
fun main() {
    val example = "kotlinは楽しい！"
    val capitalized = example.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }
    println(capitalized)  // 出力：Kotlinは楽しい！
}
```

## Deep Dive (深い潜在)
Kotlin 1.5以前では、`capitalize()` メソッドが使えました。でも、これは廃止されました。理由は簡単 — おそらくUnicodeの基準や地域設定に関連した問題です。Unicodeでは「i」の大文字が「I」じゃない言語もあるからです。

代わりに`replaceFirstChar`を使いましょう。これは最初の文字に関数を適用するので、より安全でカスタマイズ可能です。

内部的には、`titlecase()` が文化圏に敏感で、言語のルールに従って文字を大文字にします。たとえばトルコ語では "titlecase('i')" は "İ" になります。

他にも手段があります。たとえば、Apache Commons の `StringUtils.capitalize()` や Guava の `CaseFormat` を利用する方法もありますが、これらは外部ライブラリが必要となります。

## See Also (関連情報)
- Kotlin Documentation: https://kotlinlang.org/docs/reference/
- Unicode Standards: https://unicode.org/
- Apache Commons StringUtils: https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html
- Guava CaseFormat: https://guava.dev/releases/19.0/api/docs/com/google/common/base/CaseFormat.html