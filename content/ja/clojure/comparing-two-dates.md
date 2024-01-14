---
title:                "Clojure: 二つの日付の比較"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ？

Clojureを使う人々は、日常生活やビジネス上ではよく日付を比較する必要があります。日付を比較することで、ブログ投稿の順序を決めたり、データの古さをチェックしたりできます。

## 使い方

比較するための2つの日付を `LocalDate` データ型として取得します。

```Clojure
(def today (java.time.LocalDate/now)) 
;; 現在の日付を取得

(def tomorrow (.plusDays today 1))
;; 現在の日付から1日後の日付を取得
```

比較する際には、`<`、`>`、`=` 演算子を使用します。

```Clojure
(println (< today tomorrow))
;; 結果: true

(println (> today tomorrow))
;; 結果: false

(println (= today today))
;; 結果: true
```

`Range` を使用することで、指定した範囲内の日付を取得できます。

```Clojure
(def date-range (range today tomorrow))
(println date-range)
;; 結果: (#object[java.time.LocalDate "2021-09-04"]
         #object[java.time.LocalDate "2021-09-05"])
```

複数の日付を比較する場合は、`every?` 関数を使用します。

```Clojure
(def dates (list today tomorrow))
(println (every? #(> % today) dates))
;; 結果: false
```

## 深堀り

Clojureでは、日付を比較する方法は他にもたくさんあります。例えば、`compare` 関数を使用する方法や、`predicate` を使ったフィルタリングなどです。また、もし日付が文字列型であれば、`parse` を使って日付型に変換することもできます。

## 参考記事

- [Clojure Docs: LocalDate](https://clojuredocs.org/clojure.java-time/local-date)
- [Clojure Date/Time Library](https://github.com/clj-time/clj-time)
- [A comprehensive guide to working with dates and times in Clojure](https://clojureverse.org/t/a-comprehensive-guide-to-working-with-dates-and-times-in-clojure/3155)