---
date: 2024-01-20 17:28:37.329543-07:00
description: "How to: (\u65B9\u6CD5) Clojure\u306B\u306F`clj-time`\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u304C\u3042\u308A\u307E\u3059\u304C\u3001Clojure 1.10\u3067\u5C0E\u5165\
  \u3055\u308C\u305F`java.time`\u3092\u5229\u7528\u3057\u305F\u4F8B\u3092\u793A\u3057\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.522852-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Clojure\u306B\u306F`clj-time`\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u304C\u3042\u308A\u307E\u3059\u304C\u3001Clojure 1.10\u3067\u5C0E\u5165\u3055\u308C\
  \u305F`java.time`\u3092\u5229\u7528\u3057\u305F\u4F8B\u3092\u793A\u3057\u307E\u3059\
  \u3002"
title: "\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u306E\u8A08\u7B97"
weight: 26
---

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
