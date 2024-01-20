---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何それ & なぜ必要？ (What & Why?)

日付の比較とは、2つの日付が等しいか、あるいはどちらが遅いかを判断するプロセスを意味します。プログラマーが日付の比較を行う主な理由は、任意の2つのイベントがいつ発生したかを確定するためです。

## 使い方 (How to)

以下のコードスニペットは、Clojureにおける2つの日付の比較法を示しています。

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])

(def date1 (c/to-date-time "2021-01-01T12:00:00Z"))
(def date2 (c/to-date-time "2022-01-01T12:00:00Z"))

(println (t/after? date2 date1))
```

このコードは`date2`が`date1`より後かどうかを判断します。結果を示す実行結果は以下の通りです：

```Clojure
true
```

`date2`はたしかに`date1`の後に来ますので、コードは`true`を返します。

## ディープダイブ (Deep Dive)

日付の比較は、プログラマーにとって重要な基本的なタスクです。それは日付が時系列のイベントを表現する一般的な方法であるためです。

Clojure languageでは、`clj-time`ライブラリが提供する関数を使って日付を比較できます。これはJoda-Timeライブラリのクロージャーへのラッパーで、高レベルな日付と時間の操作を提供します。

比較する日付が文字列形式の場合、`clj-time.coerce`モジュールの`to-date-time`関数を使ってISO 8601形式の日付時刻に変換できます。

また、比較の代わりに`clj-time.core/difference`を使って2つの日付間の差を計算することも可能です。

## 参考資料 (See Also)

以下のリンクから、日付の比較を含むClojureの日付と時間の操作に関してさらに学ぶことができます：

1. clj-time GitHub page: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
2. Clojure for the Brave and True のパート "Date and Time": [https://www.braveclojure.com/core-functions-in-depth/#Date_and_Time](https://www.braveclojure.com/core-functions-in-depth/#Date_and_Time)