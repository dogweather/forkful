---
title:                "Clojure: 「テキストファイルを読む」"
simple_title:         "「テキストファイルを読む」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読むのに、なぜこの記事を読む必要があるのでしょうか？テキストファイルはプログラミングにおいて非常に重要な役割を果たしています。そのため、この記事ではClojure言語を使用してテキストファイルを読む方法を紹介します。

## 使い方

テキストファイルを読むには、まず`clojure.java.io`ライブラリを使用する必要があります。以下のようにコードを書くことで、テキストファイルを読み込むことができます。

```Clojure
(require '[clojure.java.io :as io])

(with-open [reader (io/reader "sample.txt")]
  (doseq [line (line-seq reader)]
    (println line)))
```

上記のサンプルコードでは、`sample.txt`というファイルを読み込んで、1行ずつ内容を出力しています。もちろん、このコードは自分のファイル名に合わせて変更する必要があります。

## ディープダイブ

テキストファイルを読む方法は様々ありますが、基本的な考え方は同じです。まずはファイルを開いて、行ごとに処理していくという流れです。ただし、ファイルを開く際にはエラー処理をきちんと行う必要があります。

また、テキストファイルを読み込むだけでなく、書き込むこともできます。`clojure.java.io`ライブラリには`writer`関数もあるため、ファイルを作成してその中に文字を書き込むこともできます。

## 参考リンク

- [Clojure - Java Interop](https://clojure.org/reference/java_interop)
- [clojure.java.io - ClojureDocs](https://clojuredocs.org/clojure.java.io/reader)
- [How to Read and Write Text Files in Clojure - Clojure for the Brave and True](https://www.braveclojure.com/reading-and-writing/)