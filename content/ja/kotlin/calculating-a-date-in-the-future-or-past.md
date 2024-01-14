---
title:                "Kotlin: 「未来または過去の日付を計算する」"
simple_title:         "「未来または過去の日付を計算する」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

私たちが日常的に日付を計算する必要はありませんが、ある時に未来や過去の日付を知りたいときがあります。この記事では、Kotlinを使って未来や過去の日付を簡単に計算する方法を紹介します。

## なぜ計算するのか

未来や過去の日付を計算する理由はさまざまです。例えば、予定が入った日付をチェックしたい場合や何日後に旅行があるか知りたい場合などです。Kotlinで日付を計算することで、簡単に答えを得ることができます。

## 計算する方法

```Kotlin
import java.time.LocalDate

fun main() {
    // 現在の日付を取得
    val currentDate = LocalDate.now()
    println("現在の日付は ${currentDate} です。")
    
    // 10日後の日付を計算
    val futureDate = currentDate.plusDays(10)
    println("10日後の日付は ${futureDate} です。")
    
    // 2週間前の日付を計算
    val pastDate = currentDate.minusWeeks(2)
    println("2週間前の日付は ${pastDate} です。")
}
```

このコードを実行すると、現在の日付から10日後と2週間前の日付を簡単に計算することができます。

```
現在の日付は 2021-08-25 です。
10日後の日付は 2021-09-04 です。
2週間前の日付は 2021-08-11 です。
```

このように、Kotlinでは日付を簡単に計算することができます。さらに、月や年を基準として計算することもできます。詳細は公式ドキュメントを参照してください。

## 詳しく見ていく

日付の計算には、Javaの`java.time`パッケージを利用します。KotlinではJavaのライブラリを直接使用することができるため、日付を操作する機能をすべて利用することができます。

さらに、日付を文字列として表示したい場合は`toString()`メソッドを、特定のフォーマットで表示したい場合は`format()`メソッドを使用することができます。また、一度に複数の日付を計算することもできます。

## See Also
- [Java8 Tutorial: DateTime](https://www.tutorialspoint.com/java8/java8_datetime_api.htm)
- [Java Date and Time API](https://www.geeksforgeeks.org/java-date-and-time-api-with-examples/?ref=rp)
- [Kotlin: Working with Dates](https://blog.kotlin-academy.com/kotlin-work-with-date-time-3135fcb5c89a?gi=5176bc069a4f)
 
以上でKotlinを使った日付の計算の紹介を終わります。日常生活で役立つ知識を身につけることができたかもしれません。ぜひ様々な場面でKotlinを活用してみてください。