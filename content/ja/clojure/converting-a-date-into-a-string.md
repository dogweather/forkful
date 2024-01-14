---
title:                "Clojure: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することの重要性は、特定の日付形式を必要とする場合や、日付を読み取りやすく表示したい場合にあります。

## 方法

```Clojure
(def today (java.time.LocalDate/now)) 
(str today) ;=> "2021-09-14" 
(format today "dd/MM/yyyy") ;=> "14/09/2021"
```

上記のコードでは、まず`java.time.LocalDate/now`を使用して今日の日付を取得し、その日付を`str`を使って文字列に変換しています。それに加えて、`format`を使って特定の日付形式に変換することもできます。

## ディープダイブ

日付を文字列に変換する際には、使用する日付ライブラリや目的によって異なる方法があります。また、ロケールやタイムゾーンなどの考慮も重要です。必要に応じて、より詳細な説明や実際のコード例を参考にしてください。

## もっと見る

- [Clojureで日付を扱う方法](https://clojure.or.jp/articles/date-handling-in-clojure/)
- [日付と時刻の操作に関するClojureのガイド](https://clojuredocs.org/clojure.java-time)