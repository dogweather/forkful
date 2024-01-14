---
title:                "Clojure: テキストファイルを作成する"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを書く理由は多々ありますが、主な目的はデータの保存や共有です。また、プログラミング言語によってはテキストファイルを読み書きすることができるため、プログラミングの学習や演習にも役立ちます。

## 作り方

まずはClojureをインストールし、ローカルにテキストファイルを作成します。その後、以下のようなコードを入力し、ファイルにテキストを書き込みます。

```Clojure
(with-open [file (java.io.FileWriter. "sample.txt")]
  (.write file "Hello World")
  (.write file "This is a sample text file"))
```

そして、ファイルを閉じるために以下のコードを追加します。

```Clojure
(.close file)
```

実行すると、"sample.txt"という名前のファイルが作成され、その中には「Hello World This is a sample text file」というテキストが記載されます。これで、テキストファイルの作成が完了しました。

## ディープダイブ

Clojureでは、より複雑なテキストファイルの作成や編集も可能です。例えば、以下のコードを使用すると、カンマ区切りのデータを含むCSVファイルを作成することができます。

```Clojure
(with-open [file (java.io.FileWriter. "data.csv")]
  (.write file "Name,Age,Country")
  (.newLine file)
  (.write file "John,25,USA")
  (.newLine file)
  (.write file "Lisa,30,Japan")
  (.newLine file)
  (.write file "Mark,28,UK"))
```

このコードを実行すると、"data.csv"というファイルが作成され、以下のような内容が記載されます。

```
Name,Age,Country
John,25,USA
Lisa,30,Japan
Mark,28,UK
```

また、既存のテキストファイルを読み込み、編集することもできます。以下のコードでは、"sample.txt"というファイルを読み込んで、最後に行を追加しています。

```Clojure
(with-open [file (java.io.FileReader. "sample.txt")]
  (let [lines (line-seq file)]
    (doall (take (- (count lines) 1) lines))
    (.write file "This is an additional line")
    (.newLine file)
    (.write file (last lines))))
```

実行すると、"sample.txt"には元々の内容に加えて「This is an additional line」という行が追加されます。

## はてな見る

- [Clojure公式サイト](https://clojure.org/)
- [Clojureドキュメント](https://clojure.org/documentation)
- [ClojureでCSVファイルを作成する方法](https://www.freecodecamp.org/news/easiest-way-to-create-and-split-a-csv-file/comment-page-1/)
- [Clojureでテキストファイルを編集する方法](https://clojuredocs.org/clojure.core/with-open)