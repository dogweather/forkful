---
title:                "テキストファイルの読み込み"
html_title:           "Clojure: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ
あなたがテキストファイルを読む上で何が重要か気になりますか？テキストファイルを読むことで、プログラムやデータを扱う上で不可欠な情報を得ることができます。この記事では、Clojureを使ってテキストファイルを読む方法をご紹介します。

## 使い方
```Clojure
(with-open [reader (clojure.java.io/reader "sample.txt")]
  (doseq [line (line-seq reader)]
    (println line)))
```

これはClojureでテキストファイルを読み込む最も基本的な方法です。`sample.txt`というファイルを開いて、それぞれの行を順に取り出して出力することができます。

```Clojure
(with-open [reader (clojure.java.io/reader "sample.txt")]
  (doall (take 5 (line-seq reader))))
```

さらに、ファイルから一部の行だけを取り出したい場合は、`take`関数を使って行数を指定することもできます。

```Clojure
(with-open [reader (clojure.java.io/reader "sample.txt")]
  (doall (filter #(re-find #"Clojure" %) (line-seq reader))))
```

あるパターンにマッチする行を取り出したい場合は、`filter`関数を使って正規表現を指定することもできます。

## 深く掘り下げる
Clojureは標準ライブラリによって、テキストファイルを扱うための便利な関数を多数提供しています。例えば、`clojure.string`モジュールにはテキストの分割や結合を行う関数が含まれています。

また、Clojureにはデータ構造としてのシーケンスがあり、これを使ってテキストファイルを処理することができます。シーケンスを操作する関数を使えば、より高度なテキストファイルの処理が可能になります。

## See Also (参考リンク)
- [Clojure Documentation](https://clojure.org/)
- [Clojure for the Brave and True](http://www.braveclojure.com/)
- [Learn Clojure in Y Minutes](https://learnxinyminutes.com/docs/ja-jp/clojure-ja/)