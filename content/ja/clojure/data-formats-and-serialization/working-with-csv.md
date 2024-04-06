---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:28.593638-07:00
description: "\u65B9\u6CD5: Clojure\u306B\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u306BCSV\u30D1\u30FC\u30B7\u30F3\u30B0\u306E\u30D3\u30EB\u30C8\u30A4\
  \u30F3\u6A5F\u80FD\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\u3053\u306E\u76EE\
  \u7684\u306B`clojure.data.csv`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3046\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002\u307E\u305A\u3001\u30D7\u30ED\u30B8\u30A7\
  \u30AF\u30C8\u306E\u4F9D\u5B58\u95A2\u4FC2\u306B\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\
  \u8FFD\u52A0\u3057\u307E\u3059\u3002 `project.clj`\u306B\u4EE5\u4E0B\u306E\u4F9D\
  \u5B58\u95A2\u4FC2\u3092\u8FFD\u52A0\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.586188-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u306B\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\
  CSV\u30D1\u30FC\u30B7\u30F3\u30B0\u306E\u30D3\u30EB\u30C8\u30A4\u30F3\u6A5F\u80FD\
  \u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\u3053\u306E\u76EE\u7684\u306B`clojure.data.csv`\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002\u307E\u305A\u3001\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u4F9D\u5B58\u95A2\
  \u4FC2\u306B\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u8FFD\u52A0\u3057\u307E\u3059."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

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
