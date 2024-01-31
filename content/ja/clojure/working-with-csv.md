---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
CSV(Comma-Separated Values)は、データをカンマで区切って保存するファイル形式だ。プログラマはCSVを使ってデータの移動、保存、解析が簡単なため。

## How to: (方法)
ClojureでCSVを扱う基本。以下に例を示す。

```Clojure
(require '[clojure.java.io :as io])
(require '[clojure.data.csv :as csv])
(require '[clojure.string :as str])

; CSVファイルを読み込む
(with-open [reader (io/reader "data.csv")]
  (doall 
    (csv/read-csv reader)))

; CSVデータを書き込む
(let [data [["name" "age" "city"] ["Alice" "30" "Tokyo"] ["Bob" "25" "Osaka"]]]
  (with-open [writer (io/writer "output.csv")]
    (csv/write-csv writer data)))
```

`read-csv`でCSVを読み込む。`write-csv`でCSVを書き込む。簡単。

## Deep Dive (掘り下げ)
CSVは長い歴史を持つ。多くのプログラムでサポートされている。JSONやXMLなどの代替手段もあるが、読み書きの速さとシンプルさでCSVが選ばれることが多い。Clojureでは`clojure.data.csv`ライブラリが使われる。ライブラリはプログラムの依存性に追加するだけ。

## See Also (関連情報)
- Clojure公式サイトのCSV関連ドキュメント: [https://clojure.github.io/clojure](https://clojure.github.io/clojure)
- `clojure.data.csv`ライブラリのガイド: [https://github.com/clojure/data.csv](https://github.com/clojure/data.csv)
- CSVについて学ぶ: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
