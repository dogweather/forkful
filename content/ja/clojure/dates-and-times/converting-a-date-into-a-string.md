---
date: 2024-01-20 17:36:23.037899-07:00
description: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u3044\u3046\u306E\u306F\u3001\u65E5\u4ED8\u306E\u30C7\u30FC\u30BF\u3092\u30C6\u30AD\
  \u30B9\u30C8\u5F62\u5F0F\u3067\u8868\u73FE\u3059\u308B\u3053\u3068\u3067\u3059\u3002\
  \u3053\u306E\u5909\u63DB\u304C\u884C\u308F\u308C\u308B\u7406\u7531\u306F\u3001\u30C7\
  \u30FC\u30BF\u306E\u53EF\u8AAD\u6027\u3092\u5411\u4E0A\u3055\u305B\u3001\u30ED\u30B0\
  \u3001\u30E1\u30C3\u30BB\u30FC\u30B8\u3001UI\u306B\u8868\u793A\u3059\u308B\u969B\
  \u306B\u5FC5\u8981\u3060\u304B\u3089\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:39.724815-07:00'
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u3044\u3046\u306E\u306F\u3001\u65E5\u4ED8\u306E\u30C7\u30FC\u30BF\u3092\u30C6\u30AD\
  \u30B9\u30C8\u5F62\u5F0F\u3067\u8868\u73FE\u3059\u308B\u3053\u3068\u3067\u3059\u3002\
  \u3053\u306E\u5909\u63DB\u304C\u884C\u308F\u308C\u308B\u7406\u7531\u306F\u3001\u30C7\
  \u30FC\u30BF\u306E\u53EF\u8AAD\u6027\u3092\u5411\u4E0A\u3055\u305B\u3001\u30ED\u30B0\
  \u3001\u30E1\u30C3\u30BB\u30FC\u30B8\u3001UI\u306B\u8868\u793A\u3059\u308B\u969B\
  \u306B\u5FC5\u8981\u3060\u304B\u3089\u3067\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

日付を文字列に変換するというのは、日付のデータをテキスト形式で表現することです。この変換が行われる理由は、データの可読性を向上させ、ログ、メッセージ、UIに表示する際に必要だからです。

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
