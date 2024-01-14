---
title:                "Clojure: 現在の日付の取得"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
なぜ現在の日付を取得するのか？
現在の日付を取得することで、プログラム内で日付を使用することができます。例えば、特定の日付を条件としてプログラムを実行することができます。

## How To
```Clojure
; 現在の日付を取得する方法
(def current-date (java.util.Date.))

; 日付のフォーマットを指定
(def formatter (java.text.SimpleDateFormat. "yyyy-MM-dd"))

; フォーマットに従って現在日付を文字列に変換
(def formatted-date (.format formatter current-date))

; 出力
(formatted-date)  ; => "2020-08-03"
```

## Deep Dive
現在の日付を取得するには、Javaの`java.util.Date`クラスを使用します。このクラスは、時刻情報を表すオブジェクトです。次に、`java.text.SimpleDateFormat`を使用して日付のフォーマットを指定します。最後に、`format`メソッドを使用して、指定したフォーマットに従って日付を文字列に変換します。

## See Also
[Clojure公式ドキュメント](https://clojure.org/index)  
[日付と時刻の操作に関するClojure Cheatsheet](https://clojure.org/guides/formats)