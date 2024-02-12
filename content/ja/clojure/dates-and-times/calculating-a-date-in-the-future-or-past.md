---
title:                "未来または過去の日付の計算"
aliases:
- ja/clojure/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:28:37.329543-07:00
model:                 gpt-4-1106-preview
simple_title:         "未来または過去の日付の計算"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
未来または過去の日付を計算するとは、特定の日付から所定の日数を加えたり引いたりして新しい日付を求めることです。プログラマーは、有効期限、イベントのスケジューリング、リマインダーなどの機能を実装する際にこれを行います。

## How to: (方法)
Clojureには`clj-time`ライブラリがありますが、Clojure 1.10で導入された`java.time`を利用した例を示します。

```clojure
(require '[clojure.java-time :as t])

;; 今日の日付を取得
(def today (t/local-date))

;; 3日後の日付を計算
(def three-days-later (t/plus-days today 3))

;; 5日前の日付を計算
(def five-days-ago (t/minus-days today 5))

;; 結果を表示
(println "Today: " today)
(println "Three days later: " three-days-later)
(println "Five days ago: " five-days-ago)
```

実行結果:

```
Today: 2023-03-30
Three days later: 2023-04-02
Five days ago: 2023-03-25
```

## Deep Dive (深掘り)
ClojureはJVMベースの言語であり、Javaの`java.time`ライブラリの利点を享受できます。`clj-time`は以前よく使われましたが、Java 8から導入された`java.time`が登場して主流となりました。

他言語では、Pythonには`datetime`モジュール、JavaScriptには`Date`オブジェクトや`moment.js`ライブラリがあります。

`java.time`は不変オブジェクト（変更不能）であり、「チェーン」メソッドを使って直感的に日付を操作できることが特徴です。ローカライズやタイムゾーンの管理も強力です。Clojureでは、このライブラリをラップして簡潔で機能豊富なAPIを提供する`clojure.java-time`というライブラリを使用することが一般的です。

## See Also (関連情報)
- Clojure公式サイト: [https://clojure.org/](https://clojure.org/)
- `clojure.java-time` GitHubページ: [https://github.com/dm3/clojure.java-time](https://github.com/dm3/clojure.java-time)
- Java `java.time`パッケージドキュメント: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
