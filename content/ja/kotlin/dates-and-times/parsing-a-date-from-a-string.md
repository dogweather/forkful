---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:40.131090-07:00
description: "\u65B9\u6CD5: Kotlin\u306F`java.time`\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\
  \u901A\u3058\u3066\u65E5\u4ED8\u306E\u89E3\u6790\u3092\u30B5\u30DD\u30FC\u30C8\u3057\
  \u3066\u304A\u308A\u3001Java 8\u3067\u5C0E\u5165\u3055\u308C\u307E\u3057\u305F\u3002\
  \u3053\u3053\u3067\u306F\u3001`LocalDateTime`\u3068\u7279\u5B9A\u306E\u30D1\u30BF\
  \u30FC\u30F3\u3092\u4F7F\u7528\u3057\u305F\u5358\u7D14\u306A\u30A2\u30D7\u30ED\u30FC\
  \u30C1\u3092\u7D39\u4ECB\u3057\u307E\u3059."
lastmod: '2024-03-13T22:44:42.079430-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\u306F`java.time`\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u901A\u3058\u3066\
  \u65E5\u4ED8\u306E\u89E3\u6790\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u304A\u308A\
  \u3001Java 8\u3067\u5C0E\u5165\u3055\u308C\u307E\u3057\u305F\u3002\u3053\u3053\u3067\
  \u306F\u3001`LocalDateTime`\u3068\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u3092\
  \u4F7F\u7528\u3057\u305F\u5358\u7D14\u306A\u30A2\u30D7\u30ED\u30FC\u30C1\u3092\u7D39\
  \u4ECB\u3057\u307E\u3059."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

## 方法:
Kotlinは`java.time`パッケージを通じて日付の解析をサポートしており、Java 8で導入されました。ここでは、`LocalDateTime`と特定のパターンを使用した単純なアプローチを紹介します:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // 出力: 2023-04-01T12:00
}
```

より柔軟性が必要な場合や、APIのような外部ソースからの日付を扱う場合は、Joda-Timeのようなサードパーティライブラリを使用することもありますが（`java.time`が堅牢になった現在はあまり一般的ではありません）、JDKによって提供される現代的なアプローチを採用することが、ほとんどのKotlinアプリケーションには推奨されます。

サードパーティライブラリを使用せずにKotlinで日付を解析するには、Java 8より前のバージョンや`java.time`をサポートしていないAndroid APIレベルでは、`SimpleDateFormat`クラスも使用できます:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // 出力はタイムゾーンに基づいて異なります。例: Sat Apr 01 12:00:00 GMT 2023
}
```

`SimpleDateFormat`を使用する場合は、解析された日付の予期しないオフセットを避けるために、常にタイムゾーンを設定することを忘れないでください。
