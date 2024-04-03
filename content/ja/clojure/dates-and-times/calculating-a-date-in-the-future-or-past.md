---
date: 2024-01-20 17:28:37.329543-07:00
description: "\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\u65E5\u4ED8\u304B\u3089\u6240\
  \u5B9A\u306E\u65E5\u6570\u3092\u52A0\u3048\u305F\u308A\u5F15\u3044\u305F\u308A\u3057\
  \u3066\u65B0\u3057\u3044\u65E5\u4ED8\u3092\u6C42\u3081\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u6709\u52B9\u671F\u9650\u3001\
  \u30A4\u30D9\u30F3\u30C8\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\
  \u30EA\u30DE\u30A4\u30F3\u30C0\u30FC\u306A\u3069\u306E\u6A5F\u80FD\u3092\u5B9F\u88C5\
  \u3059\u308B\u969B\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.576526-06:00'
model: gpt-4-1106-preview
summary: "\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\u65E5\u4ED8\u304B\u3089\u6240\
  \u5B9A\u306E\u65E5\u6570\u3092\u52A0\u3048\u305F\u308A\u5F15\u3044\u305F\u308A\u3057\
  \u3066\u65B0\u3057\u3044\u65E5\u4ED8\u3092\u6C42\u3081\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u6709\u52B9\u671F\u9650\u3001\
  \u30A4\u30D9\u30F3\u30C8\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\
  \u30EA\u30DE\u30A4\u30F3\u30C0\u30FC\u306A\u3069\u306E\u6A5F\u80FD\u3092\u5B9F\u88C5\
  \u3059\u308B\u969B\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u306E\u8A08\u7B97"
weight: 26
---

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
