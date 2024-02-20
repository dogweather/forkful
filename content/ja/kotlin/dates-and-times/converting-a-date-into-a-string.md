---
date: 2024-01-20 17:36:58.860735-07:00
description: "\u30C7\u30FC\u30BF\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u65E5\u4ED8\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\u3084\u3059\u3044\
  \u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\
  \u30ED\u30B0\u8A18\u9332\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\
  \u30A7\u30FC\u30B9\u3001\u30C7\u30FC\u30BF\u51E6\u7406\u306E\u305F\u3081\u306B\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.234535
model: gpt-4-1106-preview
summary: "\u30C7\u30FC\u30BF\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u65E5\u4ED8\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\u3084\u3059\u3044\
  \u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\
  \u30ED\u30B0\u8A18\u9332\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\
  \u30A7\u30FC\u30B9\u3001\u30C7\u30FC\u30BF\u51E6\u7406\u306E\u305F\u3081\u306B\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
データを文字列に変換するとは、日付データを読みやすいテキスト形式にすることです。ログ記録、ユーザーインターフェース、データ処理のためにプログラマはこれを行います。

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
