---
date: 2024-01-20 17:31:45.307713-07:00
description: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\u65E5\u4ED8\u304B\u3089\u6307\
  \u5B9A\u3055\u308C\u305F\u671F\u9593\u3092\u52A0\u7B97\u307E\u305F\u306F\u6E1B\u7B97\
  \u3057\u3066\u65B0\u305F\u306A\u65E5\u4ED8\u3092\u6C42\u3081\u308B\u3053\u3068\u3067\
  \u3059\u3002\u4E88\u5B9A\u7BA1\u7406\u3001\u671F\u9650\u306E\u8A08\u7B97\u3001\u8A18\
  \u5FF5\u65E5\u306E\u8FFD\u8DE1\u306A\u3069\u3001\u3055\u307E\u3056\u307E\u306A\u7406\
  \u7531\u304B\u3089\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u884C\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.085388-06:00'
model: gpt-4-1106-preview
summary: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\u65E5\u4ED8\u304B\u3089\u6307\
  \u5B9A\u3055\u308C\u305F\u671F\u9593\u3092\u52A0\u7B97\u307E\u305F\u306F\u6E1B\u7B97\
  \u3057\u3066\u65B0\u305F\u306A\u65E5\u4ED8\u3092\u6C42\u3081\u308B\u3053\u3068\u3067\
  \u3059\u3002\u4E88\u5B9A\u7BA1\u7406\u3001\u671F\u9650\u306E\u8A08\u7B97\u3001\u8A18\
  \u5FF5\u65E5\u306E\u8FFD\u8DE1\u306A\u3069\u3001\u3055\u307E\u3056\u307E\u306A\u7406\
  \u7531\u304B\u3089\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u884C\u3044\u307E\u3059\
  \u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
---

{{< edit_this_page >}}

## What & Why?（何となぜ？）

将来または過去の日付を計算するとは、特定の日付から指定された期間を加算または減算して新たな日付を求めることです。予定管理、期限の計算、記念日の追跡など、さまざまな理由からプログラマーが行います。

## How to:（やり方）

```kotlin
import java.time.LocalDate
import java.time.temporal.ChronoUnit

fun main() {
    val today = LocalDate.now()
    val tenDaysLater = today.plusDays(10)
    val twoWeeksBefore = today.minus(2, ChronoUnit.WEEKS)

    println("Today: $today") // 例：Today: 2023-04-01
    println("10 days later: $tenDaysLater") // 例：10 days later: 2023-04-11
    println("Two weeks before: $twoWeeksBefore") // 例：Two weeks before: 2023-03-18
}
```

このコードは今日の日付を取得し、10日後と2週間前の日付を計算して出力します。

## Deep Dive（掘り下げ）

日付を操作するには、過去は`java.util.Date`や`java.util.Calendar`が使われてきましたが、Java 8以降では`java.time`パッケージにある`LocalDate`、`LocalDateTime`、`ZonedDateTime`が推奨されています。これらのクラスは不変（Immutable）であり、日時操作における一般的な問題を減らします。`plus`や`minus`メソッドを利用することで、簡単に日付の加算・減算ができるようになっています。

KotlinではJavaのライブラリを利用できるため、上記のメソッドがそのまま使用可能です。他の代替手段には、オープンソースの日付・時刻ライブラリであるJoda-Timeがありますが、現在では`java.time`に移行することが一般的です。

具体的な実装には、日付の加算だけでなくビジネス日（休業日を除く）の計算や、タイムゾーンを考慮した計算など、ケースに応じたさまざまな処理が必要になることがあります。これらは`java.time`パッケージを使っても、時には追加のライブラリや自作のロジックが必要になるかもしれません。

## See Also（関連情報）

- OracleのJavaドキュメントにある`java.time`パッケージの詳細: [Java Platform, Standard Edition & Java Development Kit, Version 17 API Specification](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/time/package-summary.html)
- Threeten-Backport（古いJavaバージョン用の`java.time`バックポート）: [ThreeTen-Backport](http://www.threeten.org/threetenbp/)
- Joda-Timeプロジェクト: [Joda-Time](https://www.joda.org/joda-time/)
- Stack Overflowでの日付と時刻に関する議論: [Stack Overflow](https://stackoverflow.com/questions/tagged/datetime)
