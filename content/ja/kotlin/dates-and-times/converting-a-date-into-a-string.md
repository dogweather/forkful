---
date: 2024-01-20 17:36:58.860735-07:00
description: "How to: (\u3084\u308A\u65B9) \u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\
  \u5909\u3048\u308B\u884C\u70BA\u306F\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u521D\u671F\
  \u6BB5\u968E\u304B\u3089\u5B58\u5728\u3057\u307E\u3059\u3002Kotlin\u3067\u306F\u3001\
  `SimpleDateFormat`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.961908-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u3048\
  \u308B\u884C\u70BA\u306F\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u521D\u671F\u6BB5\u968E\
  \u304B\u3089\u5B58\u5728\u3057\u307E\u3059\u3002Kotlin\u3067\u306F\u3001`SimpleDateFormat`\
  \ \u30AF\u30E9\u30B9\u3092\u4F7F\u7528\u3057\u3001\u65E5\u4ED8\u3092\u72EC\u81EA\
  \u306E\u5F62\u5F0F\u3067\u6587\u5B57\u5217\u306B\u5909\u63DB\u3067\u304D\u307E\u3059\
  \u3002\u3053\u306E\u30AF\u30E9\u30B9\u306FJava\u304B\u3089\u7D99\u627F\u3055\u308C\
  \u3066\u3044\u308B\u305F\u3081\u3001Java\u306E\u7D4C\u9A13\u304C\u3042\u308B\u4EBA\
  \u306B\u3082\u306A\u3058\u307F\u3084\u3059\u3044\u3067\u3057\u3087\u3046\u3002`SimpleDateFormat`\u306F\
  \u67D4\u8EDF\u3067\u3059\u304C\u3001\u30B9\u30EC\u30C3\u30C9\u30BB\u30FC\u30D5\u3067\
  \u306F\u306A\u3044\u305F\u3081\u6CE8\u610F\u304C\u5FC5\u8981\u3067\u3059\u3002\u307E\
  \u305F\u3001Java 8\u304B\u3089\u306F`DateTimeFormatter`\u304C\u63A8\u5968\u3055\u308C\
  \u3066\u3044\u307E\u3059\u304C\u3001Android\u3067\u306F\u5168\u30D0\u30FC\u30B8\u30E7\
  \u30F3\u3067\u5229\u7528\u3067\u304D\u308B\u308F\u3051\u3067\u306F\u306A\u3044\u305F\
  \u3081\u3001`SimpleDateFormat`\u304C\u3088\u304F\u7528\u3044\u3089\u308C\u307E\u3059\
  \u3002\u5404\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306E\u30CB\u30FC\u30BA\u306B\u5408\
  \u308F\u305B\u3066\u9078\u629E\u3057\u307E\u3057\u3087\u3046\u3002"
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
