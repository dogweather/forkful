---
title:                "「二つの日付の比較」"
html_title:           "Clojure: 「二つの日付の比較」"
simple_title:         "「二つの日付の比較」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何をするのか & なぜ必要なのか?
日付を比較するとは、2つの日付を取り、それらが同じ日付なのか、または1つが他よりも前後するかどうかを確認することを指します。プログラマーがこの作業を行うのは、時には目的の日付範囲内で特定の操作を行う必要があるためです。

## 方法:
```Clojure
(= (java.util.Calendar/getInstance) (java.util.Calendar/getInstance)) 
;=> true

(> (java.time.LocalDate/now) (java.time.LocalDate/of 2020 11 15))
;=> true
```

## 深層掘り下げ:
- 歴史的背景: 過去には日付の比較は手動で行われており、時間や手間がかかる作業でした。しかし、現在ではコンピューターが日付データを取得し、比較することができます。
- 代替手段: 日付の比較にはさまざまな方法がありますが、Clojureでは時間やタイムゾーンの考慮をすることができるjava.timeライブラリを使用することが推奨されています。
- 実装の詳細: 日付の比較にはさまざまなプログラミング言語やライブラリがありますが、Clojureでは簡潔で分かりやすく記述することができるという利点があります。

## 関連情報:
- [JavaのCalendarクラスの比較](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html#equals-java.lang.Object-)
- [Clojureのjava.timeライブラリ](https://clojuredocs.org/clojure.java-time)
- [日付比較におけるタイムゾーンの重要性](https://qiita.com/ts-3156/items/566a71e6a2fb4b5ca3ba)