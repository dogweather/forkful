---
title:                "Kotlin: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得する理由は、日々の生活やビジネスにおいて、正確な日付を把握することが重要だからです。プログラミングにおいても、特定のタスクを実行する際に現在の日付を使用することがあります。

## 方法

例えば、Kotlinを使って現在の日付を取得するには、以下のようにコードを書きます。

```Kotlin
import java.time.LocalDate

fun main() {
    // 現在の日付を取得する
    val currentDate = LocalDate.now() 
    // 日付を表示する
    println(currentDate)
}
```

実行すると、`2021-06-29`のような出力が得られます。

## 詳細

日付を取得する際には、Javaの`java.time`パッケージを使用することが推奨されています。Kotlinでは、`LocalDate`クラスを使用することで現在の日付を取得することができます。`LocalDate.now()`メソッドを使用することで、現在の日付を取得することができます。

また、`LocalDate`クラスには、日付を操作するための様々なメソッドが用意されています。例えば、`plusDays()`を使用することで、指定した日数だけ日付を加算することができます。

## また見る

- [Kotlin公式ドキュメント: `java.time`パッケージ](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time/)
- [コーン日付操作についての記事](https://medium.com/@codemateinc/kotlin-time-manipulation-with-java-time-7d0a0fd1cb14)