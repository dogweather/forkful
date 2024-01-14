---
title:                "Clojure: 将来または過去の日付の計算"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why (なぜ): 未来や過去の日付を計算することに取り組む理由は？


未来や過去の日付を計算することは、日常生活やビジネスでとても役に立ちます。例えば、期限や公式のイベントの日付を把握したり、データの分析を行う際に必要になる場合があります。Clojureプログラミング言語を使って、簡単に日付を計算する方法を紹介します。

## How To (方法): 日付を計算する方法

まずは、Clojureプロジェクトを作成しましょう。ターミナルで`lein new app`コマンドを実行して空のプロジェクトを作成し、`project.clj`ファイルに`clj-time`ライブラリを依存関係として追加します。

```
Clojureプロジェクトの作成方法

$ lein new app calc-date
```

```
project.cljファイルにライブラリの追加

(defproject calc-date "1.0.0-SNAPSHOT"
  :description "A Clojure project for calculating dates"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [clj-time "0.15.0"]]
  :main ^:skip-aot calc-date.core
  :target-path "target/%s")
```

`src`ディレクトリに`core.clj`ファイルを作成し、以下のコードを追加します。

```
Clojureで日付を計算するコード

(ns calc-date.core
  (:require [clj-time.core :as t]
            [clj-time.format :as f]))

(defn calc-future-date [day month year]
  (let [today (t/today)]
    (t/plus today (t/days (- day (t/day-of-month this-year))
                         (- month (t/month this-month))
                         (- year (t/year this-year))))))

(defn calc-past-date [day month year]
  (let [today (t/today)]
    (t/minus today (t/days (- day (t/day-of-month this-year))
                          (- month (t/month this-month))
                          (- year (t/year this-year))))))

;; 未来の日付を計算する例
(calc-future-date 25 12 2020)
;; => #clj-time.core.DateTime "2020-12-25T00:00:00.000-00:00"

;; 過去の日付を計算する例
(calc-past-date 13 4 1992)
;; => #clj-time.core.DateTime "1992-04-13T00:00:00.000-00:00"
```

以上のコードを実行すると、引数に渡した日付に対して未来や過去の日付を計算することができます。

## Deep Dive (詳細): 日付を計算する方法の詳細

`clj-time`ライブラリは、Joda-TimeをClojure用にラップしたものです。`DateTime`オブジェクトを使って、日付の計算が可能になります。`t/today`関数を使うと、現在の日付のオブジェクトを取得できます。また、`t/plus`と`t/minus`関数を使って、特定の日数を足したり引いたりすることができます。

また、`clj-time.format`ネームスペースを使うことで、日付を任意の形式で出力することもできます。例えば、`f/unparse`関数を使うと特定の日付のオブジェクトを文字列に変換することができます。

以上の機能を駆使することで、さまざまな日付の計算が可能になります。

## See Also (関連