---
date: 2024-01-20 17:32:46.534902-07:00
description: "How to: (\u65B9\u6CD5) \u65E5\u4ED8\u306E\u6BD4\u8F03\u306FJava\u306E\
  `Date`\u30AF\u30E9\u30B9\u304B\u3089\u59CB\u307E\u308A\u307E\u3057\u305F\u3002Clojure\u306F\
  Java\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u4E0A\u3067\u52D5\u304D\u307E\
  \u3059\u304B\u3089\u3001Java\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u4F7F\u3048\
  \u307E\u3059\u3002\u305D\u308C\u306B `clj-time` \u30E9\u30A4\u30D6\u30E9\u30EA\u306F\
  Joda-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.521489-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u65E5\u4ED8\u306E\u6BD4\u8F03\u306FJava\u306E`Date`\u30AF\
  \u30E9\u30B9\u304B\u3089\u59CB\u307E\u308A\u307E\u3057\u305F\u3002Clojure\u306F\
  Java\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u4E0A\u3067\u52D5\u304D\u307E\
  \u3059\u304B\u3089\u3001Java\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u4F7F\u3048\
  \u307E\u3059\u3002\u305D\u308C\u306B `clj-time` \u30E9\u30A4\u30D6\u30E9\u30EA\u306F\
  Joda-Time\u306B\u57FA\u3065\u3044\u3066\u3044\u3066\u3001\u3088\u308A\u6271\u3044\
  \u3084\u3059\u3044\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u3092\u63D0\u4F9B\
  \u3057\u3066\u3044\u307E\u3059\u3002\u9055\u3046\u65B9\u6CD5\u3082\u3042\u308A\u307E\
  \u3059\u304C\u3001`clj-time` \u306FClojure\u30B3\u30DF\u30E5\u30CB\u30C6\u30A3\u3067\
  \u5E83\u304F\u4F7F\u308F\u308C\u3066\u3044\u307E\u3059\u3002\u3082\u3057\u7E8F\u3081\
  \u308B\u5FC5\u8981\u304C\u3042\u308B\u306A\u3089\u3001Java 8\u4EE5\u964D\u306E`java.time`\u30D1\
  \u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u3046\u3068\u3044\u3046\u9078\u629E\u80A2\u3082\
  \u3042\u308A\u307E\u3059\u304C\u3001Clojure\u3068\u306E\u7D71\u5408\u6027\u3067\u306F\
  \ `clj-time` \u306E\u65B9\u304C\u512A\u308C\u3066\u3044\u308B\u3068\u8003\u3048\u3089\
  \u308C\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

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
