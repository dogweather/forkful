---
title:                "未来または過去の日付の計算"
html_title:           "Clojure: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去に計算する必要性はさまざまです。例えば、イベントの日程を決めたり、期限を設定するために必要になることがあります。

## 方法

日付を計算するには、 `(java.util.Calendar.)` 関数を使用します。未来の日付を計算する場合は、 `(.add calendar java.util.Calendar/DAY_OF_MONTH days)` を使うことができます。過去の日付を計算する場合は、 `(.add calendar java.util.Calendar/DAY_OF_MONTH (- days))` を使用します。

```Clojure
(def now (java.util.Calendar.)) ; 今日の日付を取得

; 10日後の日付を計算
(def future-date (.add now java.util.Calendar/DAY_OF_MONTH 10))

; 5日前の日付を計算
(def past-date (.add now java.util.Calendar/DAY_OF_MONTH (- 5)))

```

### 出力例

今日の日付: 2021/06/14

10日後の日付: 2021/06/24
5日前の日付: 2021/06/09

## ディープダイブ

日付を計算する際には、 `(java.util.Calendar.)` 関数が返すオブジェクトに注意する必要があります。このオブジェクトには、`java.util.Calendar/ERA`、`java.util.Calendar/YEAR`、`java.util.Calendar/MONTH`、`java.util.Calendar/DAY_OF_MONTH`など、日付の各要素を取得するためのメソッドが用意されています。これらを組み合わせることで、さまざまな日付計算を行うことができます。

また、バグや意図しない結果を避けるために、日付のフォーマットやタイムゾーンにも注意が必要です。

## さらに見る

- [The Joy of Clojure](https://www.amazon.co.jp/Joy-Clojure-Michael-Fogus/dp/1617291412/)
- [Clojureの日付操作ライブラリ - clj-time](https://github.com/clj-time/clj-time)