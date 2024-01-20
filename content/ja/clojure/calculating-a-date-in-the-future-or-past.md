---
title:                "未来または過去の日付を計算する"
html_title:           "Clojure: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付の計算は、将来または過去の特定の日付を決定するプロセスです。プログラマーはスケジューリング、予測、及びデータ分析など、指定した日時範囲内での操作を行うためにこれを行います。

## やり方：

次のコードスニペットは、特定の日数後の日付を計算します。

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.periodic :as p])

(defn date-after-days [date days]
  (t/plus date (p/days days)))
```

この関数を使用して、10日後の日付を計算できます。

```Clojure
(println (date-after-days (t/today) 10))
```

出力:

```Clojure
2023-11-19
```

同様に、指定した日数前の日付を計算する関数を作成できます。

```Clojure
(defn date-before-days [date days]
  (t/minus date (p/days days)))
```

50日前の日付を計算するには、以下の通りです。

```Clojure
(println (date-before-days (t/today) 50))
```

出力:

```Clojure
2023-09-30
```

## 深掘り：

年月日を算出するという概念は幾千年の歴史を持ち、JulianやGregorianカレンダーなど様々なシステムが存在してきました。Clojureはこの問題を解決するため、`clj-time` ライブラリを使います。

代替として `java.time` ライブラリを使用することも可能ですが、`clj-time` ライブラリはより豊富な機能を提供します。

`date-after-days` と `date-before-days` 関数は、`t/plus` と `t/minus` 函数を用いて特定の日数を日付に加減します。これにより、指定期間後または前の日付を生成します。

## 関連リンク：

1. Clojureの日付時間操作を詳細に学ぶ: https://clojure.github.io/clojure/java.interop.html#clojure.java-time
2. Joda-Timeライブラリ: https://www.joda.org/joda-time/
3. グレゴリオ暦とユリウス暦についてもっと知る: https://www.timeanddate.com/calendar/julian-gregorian-switch.html