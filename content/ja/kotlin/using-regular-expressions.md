---
title:                "正規表現の使用"
date:                  2024-01-19
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

正規表現は文字列内のパターンを検索・置換するためのツールです。効率的にデータを操作・解析するため、プログラマーはこれを利用します。

## How to: (使い方)

```kotlin
fun main() {
    val text = "Kotlinは2021年にリリースされました。"
    val regex = Regex("[0-9]+")
    val found = regex.find(text)
    println(found?.value) // "2021"
    
    val replaced = text.replace(regex, "2022")
    println(replaced) // "Kotlinは2022年にリリースされました。"
}
```

## Deep Dive (深掘り)

歴史的に、正規表現はPerl言語で広く使われてきたため高い柔軟性と強力な機能を持っています。Kotlinでは`Regex`クラスを使用し、Javaの`java.util.regex`ライブラリを内部で利用しています。また、正規表現には多くの代替方法がありますが、パターンマッチングや文字列解析では正規表現が最適な選択肢であることが多いです。

## See Also (関連情報)

- Kotlin公式ドキュメントの正規表現: [Regular expressions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- `java.util.regex`パッケージドキュメンテーション: [java.util.regex package summary](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/regex/package-summary.html)
- 日本語での正規表現チュートリアル: [正規表現チュートリアル](https://www.javadrive.jp/regex/)
