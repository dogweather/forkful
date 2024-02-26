---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:40.131090-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3053\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u3092Date\u30AA\u30D6\u30B8\u30A7\
  \u30AF\u30C8\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\
  \u3002\u3053\u306E\u64CD\u4F5C\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u304C\u5165\u529B\
  \u3057\u305F\u65E5\u4ED8\u3084\u5916\u90E8\u30C7\u30FC\u30BF\u30BB\u30C3\u30C8\u304B\
  \u3089\u53D6\u5F97\u3057\u305F\u65E5\u4ED8\u306E\u3084\u308A\u53D6\u308A\u3092\u3059\
  \u308B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u3068\u3063\u3066\u57FA\
  \u672C\u7684\u3067\u3042\u308A\u3001\u30CB\u30FC\u30BA\u306B\u5FDC\u3058\u3066\u7C21\
  \u5358\u306B\u64CD\u4F5C\u3084\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3092\u884C\u3046\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.101887-07:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3053\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u3092Date\u30AA\u30D6\u30B8\u30A7\
  \u30AF\u30C8\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\
  \u3002\u3053\u306E\u64CD\u4F5C\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u304C\u5165\u529B\
  \u3057\u305F\u65E5\u4ED8\u3084\u5916\u90E8\u30C7\u30FC\u30BF\u30BB\u30C3\u30C8\u304B\
  \u3089\u53D6\u5F97\u3057\u305F\u65E5\u4ED8\u306E\u3084\u308A\u53D6\u308A\u3092\u3059\
  \u308B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u3068\u3063\u3066\u57FA\
  \u672C\u7684\u3067\u3042\u308A\u3001\u30CB\u30FC\u30BA\u306B\u5FDC\u3058\u3066\u7C21\
  \u5358\u306B\u64CD\u4F5C\u3084\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3092\u884C\u3046\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ?
文字列から日付を解析することは、テキストをDateオブジェクトに変換することを含みます。この操作は、ユーザーが入力した日付や外部データセットから取得した日付のやり取りをするアプリケーションにとって基本的であり、ニーズに応じて簡単に操作やフォーマットを行うことができます。

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
