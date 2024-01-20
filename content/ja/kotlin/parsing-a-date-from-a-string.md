---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:37:27.420646-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を文字列から解析するとは、特定の形式の文字列を日付オブジェクトに変換することです。これにより、ソフトウェアは日付データを比較、操作、保存できるようになります。

## How to: (方法)
```kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-04-01"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val date = LocalDate.parse(dateString, formatter)

    println("Year: ${date.year}, Month: ${date.monthValue}, Day: ${date.dayOfMonth}")
}
```
実行結果:
```
Year: 2023, Month: 4, Day: 1
```

## Deep Dive (深い潜入)
日付解析はJava 8の登場と共に大きく改善しました。それ以前は、`SimpleDateFormat`クラスが使われていましたが、不変でなくスレッドセーフでないという問題点がありました。`DateTimeFormatter`はこれらの問題を解決し、より直感的で簡単に日付を操作できるようになりました。

代替手段として`SimpleDateFormat`がありますが、演算の速度やスレッドセーフの面で`DateTimeFormatter`が推奨されます。また、Joda-Timeという外部ライブラリも存在しますが、Java 8以降は標準ライブラリの使用が推奨されています。

日付文字列の解析は、形式を指定することが重要です。`DateTimeFormatter`はパターンを指定することで、入力される日付形式を正確に理解することができます。

## See Also (関連情報)
- [Oracle JavaDocs: DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Baeldung Tutorial on DateTimeFormatter](https://www.baeldung.com/java-datetimeformatter)