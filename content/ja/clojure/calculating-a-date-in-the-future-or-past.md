---
title:                "Clojure: 将来や過去の日付を計算する"
simple_title:         "将来や過去の日付を計算する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日常生活では、将来や過去の日付を計算する必要がある時があります。例えば、誕生日や年末に次の年のカレンダーを見るときなどです。Clojureを使うと、簡単に日付の計算ができるので便利です。

## 方法

まずは、`clj-time`というライブラリをインストールする必要があります。このライブラリを使うと、Clojureで日付を扱うことができます。

```Clojure
(require '[clj-time.core :as time])

(time/plus (time/today) (time/days 10))
```
このコードを実行すると、今日から10日後の日付が表示されます。ここでは、`today`関数と`plus`関数を使っています。`days`関数は、日付を表す単位を指定することができます。

```Clojure
(time/minus (time/today) (time/months 6))
```
これは、今日から6か月前の日付を表示します。

## 深堀り

日付の計算では、`days`や`months`以外にも、`hours`や`minutes`などの単位を使うことができます。また、`plus`や`minus`以外にも、`from-now`や`from-now`関数を利用することで、特定の日付からの経過時間を計算することもできます。

また、`clj-time`ライブラリでは、時間帯や曜日などの情報も取得することができます。さまざまな日付の計算に役立つ関数が多数用意されているので、ぜひ試してみてください。

## 関連リンク

- [clj-time library](https://github.com/clj-time/clj-time)
- [Clojure docs on date calculations](https://clojuredocs.org/clojure.core/today)
- [TutorialsPoint on date calculations in Clojure](https://www.tutorialspoint.com/clojure/clojure_working_with_date.htm)

## 参考リンク

さまざまな日付計算の例や、より詳しい説明については、以下のリンクを参考にしてください。

- [Clojure Date and Time Manipulation](https://clojuredocs.org/clojure.java-time)
- [Clojure for Data Science: Dates and Timestamps](https://clojureforsyntax.wordpress.com/2014/10/08/clojure-for-data-science-dates-and-timestamps/)