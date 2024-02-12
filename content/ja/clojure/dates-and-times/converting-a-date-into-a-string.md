---
title:                "日付を文字列に変換する"
aliases: - /ja/clojure/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:23.037899-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を文字列に変換する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-date-into-a-string.md"
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
