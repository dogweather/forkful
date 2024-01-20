---
title:                "未来または過去の日付の計算"
html_title:           "Kotlin: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何それ？ & なぜ？  (What & Why?)
日付計算は、未来や過去の特定の日付を見つける技術です。プログラマーは、締め切りを管理するため、数日後の日付を決定するため、有効期限をチェックするためなどの理由でこれを行います。

## 使い方： (How to:)
以下は、未来の日付を計算するKotlinのコード例です。
```Kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    val futureDate = today.plusDays(10)

    println("Today is: $today")
    println("10 days from now will be: $futureDate")
}
```
このコードを実行すると以下のような出力が得られます。
```Kotlin
Today is: 2022-05-18
10 days from now will be: 2022-05-28
```

過去の日付を計算するには、次のように `minusDays` 関数を使用します。
```Kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    val pastDate = today.minusDays(10)

    println("Today is: $today")
    println("10 days ago was: $pastDate")
}
```
このコードの出力は以下のようになります。
``` Kotlin
Today is: 2022-05-18
10 days ago was: 2022-05-08
```

## より詳しい情報 (Deep Dive)
日付計算に関する詳細：

1. 歴史的な背景： かつて、日付計算はほとんど手動で行わなければなりませんでした。しかし、Java 8を元にしたKotlinではLocalDateクラスによって簡単に処理できます。

2. 代替案： Java 8以前の場合、CalendarまたはDateクラスを使用します。これらのクラスには多くの問題点がありましたがKotlinのLocalDateクラスはこれらの問題点を解消しました。

3. 実装詳細： KotlinのLocalDateクラスには、plusDays、minusDays、plusMonths、minusMonthsなどのメソッドがあり、これを使うことにより日付の計算が容易になります。

## 関連記事・参考情報セクション (See Also):
関連リンクをご覧ください：

1. Kotlin公式ドキュメンテーション (Kotlin official documentation): [https://kotlinlang.org/docs/reference/](https://kotlinlang.org/docs/reference/)

2. LocalDateクラス (LocalDate Class): [https://kotlinlang.org/api/latest/jvm/stdlib/java.time/-local-date/](https://kotlinlang.org/api/latest/jvm/stdlib/java.time/-local-date/)

3.  JavaのDateとCalendarクラスについて (About Java's Date and Calendar Classes): [https://docs.oracle.com/javase/tutorial/datetime/iso/datetime.html](https://docs.oracle.com/javase/tutorial/datetime/iso/datetime.html)