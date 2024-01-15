---
title:                "テキストファイルの作成"
html_title:           "Clojure: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

テキストファイルを書くことの価値は、プログラムやデータを保存するために使うことができることです。また、これらのファイルを共有したり、後で参照することができるため、開発やデータ分析にとって重要な要素となります。

## How To

テキストファイルを書くためには、まず ```with-open``` 関数を使ってファイルを開きます。その後、```with-open``` 内でベクターやマップなどのデータを定義し、最後にデータをファイルに書き込みます。

```Clojure
(with-open [file (clojure.java.io/writer "sample.txt")]
  (let [data [1 2 3]
        map {:a 1 :b 2}]
    (spit file data)
    (spit file map)))
```

これにより、```sample.txt``` という名前のファイルが作成され、データが次のように出力されます。

```Clojure
1 2 3
{:a 1 :b 2}
```

## Deep Dive

テキストファイルは、プログラム内で簡単に扱える形式で保存することができます。また、Clojureでは、```clojure.core``` ライブラリに含まれる多数の関数を使用して、ファイルの内容を読み込んだり、編集したりすることができます。これにより、データ分析や文書生成などのさまざまな用途に使用することができます。

## See Also

- [Clojure core library](https://clojure.github.io/clojure/clojure.core-api.html)
- [Clojure data structures](https://clojure.org/reference/data_structures)