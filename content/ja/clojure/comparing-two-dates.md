---
title:                "Clojure: 2つの日付の比較"
simple_title:         "2つの日付の比較"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

こんにちは、みなさん。今日は、 Clojure プログラミング言語についてのブログ投稿をお届けします。今回のテーマは「日付の比較」です。

## Why
日付の比較を行う理由は、日々の生活において非常に重要です。例えば、特定の日付にイベントを予定したり、過去の日付と現在の日付を比較したりする必要があったりします。Clojure は日付の比較を行うためのさまざまな方法を提供しており、便利な機能です。

## How To
Clojure で日付の比較を行うには、まずは `clojure.java-time` ライブラリをプロジェクトに追加する必要があります。そして、比較したい二つの日付を `java.time.LocalDate` オブジェクトに変換します。

```Clojure
(require '[clojure.java-time :as t])

(def date1 (t/to-local-date "2020-05-15"))
(def date2 (t/to-local-date "2020-05-20"))

```

比較したい日付が正しいデータ型になったら、`java.time.LocalDate/compareTo` 関数を使用して比較を行うことができます。この関数は、二つの日付を比較し、数値を返します。もし、date1 が date2 よりも過去の日付であれば、-1 を、同じ日付であれば 0 を、過去であれば 1 を返します。

```Clojure
;; 日付が同じ場合
(.compareTo date1 date2) ;=> 0

;; date1 が過去の日付の場合
(.compareTo date1 date2) ;=> -1

;; date2 が過去の日付の場合
(.compareTo date1 date2) ;=> 1
```

また、日付を文字列として比較することもできます。その場合、`java.time.LocalDate/parse` 関数を使用して、文字列から `java.time.LocalDate` オブジェクトを作成します。

```Clojure
(def date3 (t/parse "2020-05-10"))
(def date4 (t/parse "2020-05-15"))

;; date3 が過去の日付の場合
(.compareTo date3 date4) ;=> -1
```

## Deep Dive
日付の比較にはさまざまなケースがあります。もし、今回紹介した方法でうまくいかない場合は、`java.time.LocalDate/equals` 関数を使用して、二つの日付が同じかどうかを確認することができます。また、日付の年や月、日にちを個別に比較するためには、`java.time.LocalDate/getYear`、`java.time.LocalDate/getMonth`、`java.time.LocalDate/getDayOfMonth` 関数を使用することもできます。

## See Also
- [clojure.java-time ライブラリ](https://clojure.github.io/java-time/)
- [Java Time API ドキュメント](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java 8 日付と時刻 API を Clojure で使う](https://qiita.com/totemcaf/items/edc35d20f9cb0476ec69)