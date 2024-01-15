---
title:                "将来または過去の日付の計算"
html_title:           "Kotlin: 将来または過去の日付の計算"
simple_title:         "将来または過去の日付の計算"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

今日の日付を計算する方法 - Kotlin プログラミング記事

## Why
日付を計算することは、将来のイベントをプランするためや、過去の出来事を振り返るために役立ちます。この記事では、Kotlin を使って簡単に日付の計算ができる方法をご紹介します。

## How To
```Kotlin 
// 現在の日付を取得
val currentDate = Calendar.getInstance().time

// 1ヶ月後の日付を計算
val futureDate = Calendar.getInstance()
futureDate.add(Calendar.MONTH, 1)

// 1年前の日付を計算
val pastDate = Calendar.getInstance()
pastDate.add(Calendar.YEAR, -1)

// 日付のフォーマットを指定
val dateFormatter = SimpleDateFormat("yyyy/MM/dd")

// 結果を出力
println("現在の日付：" + dateFormatter.format(currentDate))
println("1ヶ月後の日付：" + dateFormatter.format(futureDate.time))
println("1年前の日付：" + dateFormatter.format(pastDate.time))
```

### 実行結果
現在の日付：2021/07/25
1ヶ月後の日付：2021/08/25
1年前の日付：2020/07/25

## Deep Dive
日付を計算する際、Calendar クラスを使用します。このクラスには、様々なメソッドがあり、特定の日付の部分を変更することができます。また、SimpleDateFormat クラスを使用することで、日付のフォーマットを自由に指定することができます。

See Also
- [Kotlin 公式ドキュメント](https://kotlinlang.org/docs/home.html)
- [Java Calendar クラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java SimpleDateFormat クラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)