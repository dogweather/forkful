---
title:                "日付を比較する"
aliases:
- /ja/clojure/comparing-two-dates.md
date:                  2024-01-20T17:32:46.534902-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を比較する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付の比較は二つの日付を見て、どちらが過去か未来か、あるいは同じか判定するプロセスです。日付をソートしたり、期間を計算したり、スケジューリングのロジックを実装する際に、プログラマーはこれをよく行います。

## How to: (方法)
```Clojure
;; 日付ライブラリをインポート
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

;; 日付を作成
(def date1 (coerce/to-date-time "2023-02-20T00:00:00.000Z"))
(def date2 (coerce/to-date-time "2023-02-21T00:00:00.000Z"))

;; 日付を比較
;; date1がdate2よりも前か？
(time/before? date1 date2)   ; => true

;; date1とdate2が同じ日付か？
(time/equal? date1 date2)    ; => false

;; date1がdate2よりも後か？
(time/after? date1 date2)    ; => false
```

## Deep Dive (深掘り)
日付の比較はJavaの`Date`クラスから始まりました。ClojureはJavaプラットフォーム上で動きますから、Javaのライブラリが使えます。それに `clj-time` ライブラリはJoda-Timeに基づいていて、より扱いやすいインターフェースを提供しています。違う方法もありますが、`clj-time` はClojureコミュニティで広く使われています。もし纏める必要があるなら、Java 8以降の`java.time`パッケージを使うという選択肢もありますが、Clojureとの統合性では `clj-time` の方が優れていると考えられます。

## See Also (関連情報)
- 完全な `clj-time` ドキュメンテーション: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Javaの日付と時刻APIガイド: [https://docs.oracle.com/javase/tutorial/datetime/](https://docs.oracle.com/javase/tutorial/datetime/)
- Clojure公式サイト: [https://clojure.org/](https://clojure.org/)
