---
title:                "「CSVとの作業」"
html_title:           "Clojure: 「CSVとの作業」"
simple_title:         "「CSVとの作業」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜ

CSVファイルを使ったプログラミングに取り組むメリットは多くあります。例えば、表形式のデータを簡単に扱えるため、データ処理や分析に便利です。また、多くのプログラミング言語でサポートされているため、他の言語に移植することも容易です。

## 使い方

CSVファイルをClojureで処理する方法を見ていきましょう。まず、CSVファイルを読み込むためには、clojure.data.csvライブラリを使用します。

```Clojure
(require '[clojure.data.csv :as csv])
```

次に、CSVファイルを読み込み、各行をベクターとして取得するコードを示します。

```Clojure
(with-open [file (clojure.java.io/reader "data.csv")]
  (doall (csv/read-csv file)))
```

これで、CSVファイルの中身をベクターのリストとして取得できます。例えば、以下のようなCSVファイルがあったとします。

```
Name, Age, Occupation
John, 30, Engineer
Mary, 25, Writer
```

上記のコードを実行すると、以下のような出力が得られます。

```
[["Name" "Age" "Occupation"]
["John" "30" "Engineer"]
["Mary" "25" "Writer"]]
```

また、CSVファイルに書き込むには、以下のようにコードを書きます。

```Clojure
(with-open [file (clojure.java.io/writer "output.csv")]
  (csv/write-csv file [["Name" "Age" "Occupation"] ["John" "30" "Engineer"] ["Mary" "25" "Writer"]]))
```

これで、output.csvという名前のファイルが作成され、上記の例と同じ内容が書き出されます。

## ディープダイブ

CSVファイルを扱う際によく使われる関数の一つに、`clojure.data.csv/drop-first-row`があります。これは、最初の行を除いたベクターのリストを返します。

また、CSVファイルの中身をマップとして取得するためには、`clojure.data.csv/parse-csv`を使用します。これは、列名をキーとして行の値をマップに格納します。

さらに、CSVファイルをデータベースとして扱うために、`clojure.data.csv/as-records`関数があります。これは、CSVファイルをレコードのシーケンスとして返します。

## See Also

- [clojure.data.csv ドキュメント](https://clojuredocs.org/clojure.data.csv)
- [Clojureで表を操作する方法](https://nayasasiken.hatenablog.com/entry/How_to_manipulate_tables_in_Clojure)