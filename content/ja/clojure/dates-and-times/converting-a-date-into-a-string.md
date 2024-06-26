---
date: 2024-01-20 17:36:23.037899-07:00
description: "How to (\u3084\u308A\u65B9): Clojure\u3067\u306F\u3001`clj-time`\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u7528\u3044\u305F\u308A\u3001Java\u306E`SimpleDateFormat`\u30AF\
  \u30E9\u30B9\u3092\u5229\u7528\u3057\u305F\u308A\u3057\u3066\u65E5\u4ED8\u3092\u6587\
  \u5B57\u5217\u306B\u5909\u63DB\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.520446-06:00'
model: gpt-4-1106-preview
summary: "Clojure\u3067\u306F\u3001`clj-time`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u7528\
  \u3044\u305F\u308A\u3001Java\u306E`SimpleDateFormat`\u30AF\u30E9\u30B9\u3092\u5229\
  \u7528\u3057\u305F\u308A\u3057\u3066\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\
  \u63DB\u3057\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to (やり方):
Clojureでは、`clj-time`ライブラリを用いたり、Javaの`SimpleDateFormat`クラスを利用したりして日付を文字列に変換します。

```clojure
;; clj-timeライブラリを使用する例（外部ライブラリのため、事前に追加が必要）
(require '[clj-time.format :as fmt])
(require '[clj-time.core :as time])

(let [formatter (fmt/formatters :basic-date-time)
      date-str (fmt/unparse formatter (time/now))]
  date-str)
;; 出力: "20231205T010203Z" (実行する日時によって異なります)

;; JavaのSimpleDateFormatを直接使う例
(import 'java.text.SimpleDateFormat)
(import 'java.util.Date)

(let [formatter (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss")
      date-str (.format formatter (Date.))]
  date-str)
;; 出力: "2023-12-05T01:02:03" (実行する日時によって異なります)
```

## Deep Dive (深掘り):
日付の文字列変換は、プログラミングの歴史で長い間存在しています。`clj-time`はJoda-Timeライブラリに基づいており、Clojureの日付と時刻の操作を簡単にしてくれます。しかしJava 8以降、`java.time`パッケージ（JSR-310）が導入され、より便利なAPIが提供されています。`clj-time`は過去のプロジェクトでよく見かけるものですが、新しいプロジェクトでは`java.time`パッケージを使うことが推奨されています。変換に際しては、ロケール、タイムゾーン、形式を正しく指定することが重要です。

## See Also (参照):
- clj-time GitHub - https://github.com/clj-time/clj-time
- SimpleDateFormat documentation - https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
- Clojureでのjava.timeの使い方 - https://clojure.org/guides/deps_and_cli#_using_java_time
