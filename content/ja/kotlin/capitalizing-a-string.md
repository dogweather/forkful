---
title:                "「文字列の大文字化」"
html_title:           "Kotlin: 「文字列の大文字化」"
simple_title:         "「文字列の大文字化」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何？なぜ？

文字列の大文字化とは何かを説明するために、Kotlinの最新バージョンを使用して説明します。 プログラマーがなぜ文字列を大文字化するのかも説明します。

## 方法：

```Kotlin
fun main() {
    val str = "hello, world!"
    
    // 使用するメソッドを呼び出して、文字列を大文字に変換する
    val uppercase = str.toUpperCase()
    
    println(uppercase) // 出力: HELLO, WORLD!
}
```

## 深堀り：

文字列の大文字化は、文字列をすべて大文字に変換することを意味します。これは主に文字列の比較や表示のために使用されます。代替手段として、プログラマーは文字列をループで回して個々の文字を大文字に変換することもできますが、メソッドを使う方が簡単です。Kotlinでは、文字列の大文字化には`toUpperCase()`メソッドが用意されています。 

## 参考：

- [Kotlinの文字列操作](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)
- [Kotlinの文字列メソッド](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)