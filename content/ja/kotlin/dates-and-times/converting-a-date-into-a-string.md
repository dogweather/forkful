---
date: 2024-01-20 17:36:58.860735-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.082792-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to: (やり方)
```kotlin
import java.text.SimpleDateFormat
import java.util.*

fun main() {
    val date = Date()
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    val dateStr = formatter.format(date)
    println(dateStr) // 例: 2023-03-26 15:00:00
}
```

## Deep Dive (深掘り)
日付を文字列に変える行為はプログラムの初期段階から存在します。Kotlinでは、`SimpleDateFormat` クラスを使用し、日付を独自の形式で文字列に変換できます。このクラスはJavaから継承されているため、Javaの経験がある人にもなじみやすいでしょう。`SimpleDateFormat`は柔軟ですが、スレッドセーフではないため注意が必要です。また、Java 8からは`DateTimeFormatter`が推奨されていますが、Androidでは全バージョンで利用できるわけではないため、`SimpleDateFormat`がよく用いられます。各プログラマーのニーズに合わせて選択しましょう。

## See Also (関連項目)
- [SimpleDateFormatの公式ドキュメント](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Java8のDateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Kotlinの日付と時刻の使い方](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/)
