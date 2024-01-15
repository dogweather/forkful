---
title:                "「2つの日付を比較する」"
html_title:           "Clojure: 「2つの日付を比較する」"
simple_title:         "「2つの日付を比較する」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付を比較することの利点を説明します。日付を比較することで、特定の期間の経過を把握したり、イベントの予定を立てたりすることができます。

## 使い方

日付を比較するには、`<`、`>`、`=`のような演算子を使用します。例えば、`(< "2021-10-01" "2021-10-15")`は、1つ目の日付が2つ目の日付よりも前であるかどうかを判断します。

```Clojure
;; 今日の日付を取得
(def today (java.time.LocalDate/now))

;; 明日の日付を取得
(def tomorrow (.plusDays today 1))

;; 日付を比較する
(< tomorrow today)
=> false
(< today tomorrow)
=> true
```

## 詳細を調べる

日付を比較する際には、日付のフォーマットが重要です。特に、時刻やタイムゾーンを含む日付を比較する場合は、正しいフォーマットを指定する必要があります。

また、Clojureでは、`java.time`ライブラリを使用して日付を扱います。このライブラリには、日付の加算や減算、フォーマット変換など、便利な機能がたくさん用意されています。

## 参考リンク

- [Clojureの日付操作](https://clojuredocs.org/clojure.instant)
- [java.timeライブラリのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [よく使われる日付のフォーマット](https://docs.oracle.com/javase/tutorial/datetime/formats/index.html)

## 参考

日付を比較する際には、正しいフォーマットを指定するというポイントが重要です。また、さまざまな日付の操作を行うために、`java.time`ライブラリを使用することが役立つでしょう。ぜひ、日常的なプログラミングでこの知識を活用してみてください。