---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:28.593638-07:00
description: "CSV(Comma-Separated\u2026"
lastmod: '2024-03-13T22:44:41.586188-06:00'
model: gpt-4-0125-preview
summary: "CSV(Comma-Separated Values)\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u3053\
  \u3068\u306F\u3001\u30B9\u30D7\u30EC\u30C3\u30C9\u30B7\u30FC\u30C8\u306E\u30C7\u30FC\
  \u30BF\u3068\u540C\u69D8\u306B\u3001\u884C\u3068\u5217\u3068\u3057\u3066\u69CB\u9020\
  \u5316\u3055\u308C\u305F\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u306E\u89E3\u6790\
  \u3084\u751F\u6210\u3092\u542B\u307F\u307E\u3059\u3002\u3053\u306E\u30D7\u30ED\u30BB\
  \u30B9\u306F\u3001CSV\u304C\u8EFD\u91CF\u3067\u76F8\u4E92\u904B\u7528\u53EF\u80FD\
  \u306A\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3068\u3057\u3066\u5E83\u304F\u63A1\u7528\
  \u3055\u308C\u3066\u3044\u308B\u305F\u3081\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u3001\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\u9593\u306E\u30C7\u30FC\u30BF\
  \u4EA4\u63DB\u3084\u30C7\u30FC\u30BF\u5909\u63DB\u30BF\u30B9\u30AF\u306B\u306F\u6B20\
  \u304B\u305B\u307E\u305B\u3093\u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 何となぜ?

CSV(Comma-Separated Values)ファイルを扱うことは、スプレッドシートのデータと同様に、行と列として構造化されたテキストデータの解析や生成を含みます。このプロセスは、CSVが軽量で相互運用可能なフォーマットとして広く採用されているため、アプリケーション、データベース間のデータ交換やデータ変換タスクには欠かせません。

## 方法:

### CSVファイルの読み込み
Clojureには、標準ライブラリにCSVパーシングのビルトイン機能はありませんが、この目的に`clojure.data.csv`ライブラリを使うことができます。まず、プロジェクトの依存関係にライブラリを追加します。

`project.clj`に以下の依存関係を追加します。
```clojure
[clojure.data.csv "1.0.0"]
```
CSVファイルを読み込んで各行を印刷するには:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "path/to/yourfile.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
これにより、CSVの各行がClojureのベクタとして出力されます。

### CSVファイルへの書き込み
CSVファイルにデータを書き込むには、同じ`clojure.data.csv`ライブラリを使用できます:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "name" "age"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "path/to/outputfile.csv")]
    (csv/write-csv writer data)))
```
これにより、`outputfile.csv`が作成され（または上書きされ）、指定されたデータで満たされます。

### サードパーティライブラリの使用: `clojure.data.csv`

ClojureにおけるCSV処理のための最も単純なライブラリとして`clojure.data.csv`が挙げられますが、特別な文字や非標準の区切り文字を含むCSVを扱うような、より複雑なタスクに対しては、エコシステム内の追加のオプションを探索したり、Apache Commons CSVなどのJavaインタロプライブラリを検討することもあります。しかし、Clojureにおけるほとんどの標準的なCSV処理タスクについては、`clojure.data.csv`がシンプルかつ効果的なツールセットを提供します。
