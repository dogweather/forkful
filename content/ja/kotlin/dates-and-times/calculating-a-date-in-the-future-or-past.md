---
date: 2024-01-20 17:31:45.307713-07:00
description: "How to:\uFF08\u3084\u308A\u65B9\uFF09 \u3053\u306E\u30B3\u30FC\u30C9\
  \u306F\u4ECA\u65E5\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3057\u300110\u65E5\u5F8C\
  \u30682\u9031\u9593\u524D\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\u3057\u3066\u51FA\u529B\
  \u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.963946-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

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
