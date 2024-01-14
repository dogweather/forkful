---
title:                "Kotlin: 文字列の先頭を大文字化する"
simple_title:         "文字列の先頭を大文字化する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# キャピタライズ(Capitalize)する理由
文字列をキャピタライズする理由はさまざまです。例えば、データベースの値を正しく表示するためや、プログラムの見栄えを良くするためなどで利用されます。

## キャピタライズの方法
```Kotlin
fun capitalizeString(str: String): String {
    // 文字列の最初の文字だけを大文字にする
    return str.substring(0, 1).toUpperCase() + str.substring(1)
}

val input1 = "apple"
val output1 = capitalizeString(input1)

val input2 = "kotlin"
val output2 = capitalizeString(input2)

println(output1) // Output: Apple
println(output2) // Output: Kotlin
```
このように、Kotlinでは`toUpperCase()`と`substring()`を使うことで簡単に文字列をキャピタライズすることができます。

## キャピタライズの深い掘り下げ
文字列をキャピタライズする方法はさまざまですが、Kotlinでは`toUpperCase()`だけでなく、`capitalize()`というメソッドも提供されています。これは文字列の最初の文字だけを大文字にするだけでなく、最初以外の文字は小文字に変換されます。

また、Kotlinでは文字列が変更可能な`MutableString`と変更不可な`String`という2つのタイプがあります。`toUpperCase()`メソッドは`MutableString`のみで使うことができ、`capitalize()`メソッドはどちらでも使うことができます。

# この記事を参考にする
* [Kotlin Strings](https://kotlinlang.org/docs/basic-syntax.html#escaping-for-raw-strings)
* [Kotlin Library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)