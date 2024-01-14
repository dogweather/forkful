---
title:                "Clojure: テキストファイルの読み込み"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

読み込み専用のテキストファイルを操作することは、プログラミングで重要なスキルです。特に、データ分析やドキュメント処理などの領域では、テキストファイルを読み書きする必要があります。この記事では、Clojureでテキストファイルを読み込む方法とその重要性について説明します。

## 方法

Clojureでは、`slurp`関数を使用してファイルの内容を読み込むことができます。例えば、以下のコードを使用することで、テキストファイルを読み込み、その内容を出力することができます。

```Clojure
(def content (slurp "sample.txt"))
(println content)
```

また、ファイルの行単位で読み込むこともできます。`line-seq`関数を使用して、`slurp`関数と同様の動作を実現できます。以下のコードは、ファイルを行ごとに読み込み、それぞれの行を出力します。

```Clojure
(def lines (line-seq (clojure.java.io/reader "sample.txt")))
(doseq [line lines]
  (println line))
```

## ディープダイブ

テキストファイルを操作する際には、ファイルのエンコーディングにも注意が必要です。Clojureでは、`:encoding`オプションを指定することで、ファイルのエンコーディングを設定することができます。デフォルトでは、UTF-8が使用されますが、必要に応じて設定を変更することができます。

また、ファイルの存在をチェックする必要もあります。`exists?`関数を使用することで、ファイルが存在するかどうかを確認することができます。

## 参考リンク

- [Clojureドキュメント: slurp, line-seq関数](https://clojuredocs.org/clojure.core/slurp, line-seq)
- [Effective Programming: 読み込み専用のテキストファイルを処理する](https://github.com/FranklinChen/data-science-with-clojure/blob/master/code/src/effective_programming/file-io-basics/text-file/slurp-edn.clj)
- [日本Clojureユーザ会: 読み込み専用のテキストファイルを操作する](https://clojure.jp/reading-text-files)

## 関連リンク