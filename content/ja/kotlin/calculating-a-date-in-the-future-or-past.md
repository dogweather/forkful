---
title:                "未来や過去の日付を計算する"
html_title:           "Kotlin: 未来や過去の日付を計算する"
simple_title:         "未来や過去の日付を計算する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何でも？: 

日付の未来や過去の計算とは、指定した日付から一定の日数を加算または減算して得られる日付のことです。プログラマーがこのような計算を行うのは、日付を操作する必要があるさまざまなアプリケーションやゲームなどを開発する際に便利だからです。

## 方法: 

```Kotlin
//今から1日後
val today = LocalDate.now()
val tomorrow = today.plusDays(1)
println(tomorrow)

//今から1年前
val today = LocalDate.now()
val lastYearToday = today.minusYears(1)
println(lastYearToday)
```

出力結果：
```
2021-10-10 //今日の次の日付
2019-10-10 // 一年前の今日の日付
```

## もっと詳しく: 

日付の計算は、古代から計算機の発明前まで遡ることができます。人々は、自然現象や季節の変化に対する調整として、日付を計算する必要がありました。しかし、計算機が発明されると、人々はより正確かつ迅速に日付の計算を行うことができるようになりました。

代替として、日付の計算には多くのライブラリやアプリケーションがあります。しかし、最近のプログラミング言語は、日付や時間の操作に特化した便利なメソッドや組み込み関数を提供しており、より簡単に日付の計算を行うことができます。

Kotlinでは、日付や時間を扱うための標準ライブラリがあります。また、Javaの日付と時刻に関するAPIを使用することもできます。

## 他に見るもの: 

- [Kotlin 公式ドキュメント](https://kotlinlang.org/docs/reference/datetime.html)
- [Java 日付と時刻 API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [時計アプリケーションの作成チュートリアル](https://kotlinlang.org/docs/tutorials/java-date-time-api.html)