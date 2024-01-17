---
title:                "csvを使ったプログラミング"
html_title:           "Clojure: csvを使ったプログラミング"
simple_title:         "csvを使ったプログラミング"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## これは何ですか？
CSVとは、Comma-Separated Valuesの略称で、データをカンマで区切って保存する形式のことです。プログラマーがCSVを扱う理由は、データをテキストファイルに保存することで、より簡単にデータの管理や共有ができるからです。

## 使い方：
```Clojure
(require '[clojure.data.csv :as csv])

;; CSVファイルを読み込む
(csv/read-csv "data.csv")

;; CSVファイルを書き込む
(csv/write-csv "data.csv" [["Apple", "25"], ["Banana", "12"], ["Orange", "8"]])

;; CSVファイルをマップとして読み込む
(csv/read-csv "data.csv" :key-fn keyword)

;; マップをCSVフォーマットの文字列に変換する
(csv/generate-csv (csv/split-lines "Name, Age\nJohn, 26\nJane, 30"))
```
出力：
```
([Apple 25] [Banana 12] [Orange 8])
([:Name :Age] [:John "26"] [:Jane "30"])
"Name, Age\nJohn, 26\nJane, 30"
```

## 深く掘り下げる：
- CSVは1972年に米国で開発された、テキストベースのデータ形式です。
- CSVを扱う他の方法としては、ExcelやGoogle Sheetsなどのスプレッドシートソフトウェアを使用する方法があります。
- ```clojure.data.csv```ライブラリでは、データが入力されると自動的にCSVの形式に変換されるようになっています。

## 関連情報：
- [clojure.data.csvドキュメンテーション](https://clojure.github.io/data.csv/)
- [RFC4180 - CSVフォーマットの仕様書](https://tools.ietf.org/html/rfc4180)
- [ExcelやGoogle Sheetsなどのスプレッドシートソフトウェアを使用する方法](https://support.google.com/docs/answer/40608?hl=ja)