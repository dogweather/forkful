---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何これとその理由？

文字列の結合、これは一つまたはそれ以上の文字列を結びつけるプロセスのことを指します。なぜプログラマーはこれを行うのか？それは複数の情報を一つの形式で表示したい場合や、大量の小さな文字列を一つまとまった文字列に加工する時です。

## どうやって？

Clojureでは `str` 関数を使って文字列の結合を行います：

```Clojure
(str "Hello, " "world!")
```
上記のコードは "Hello, world!" を出力します。また、リスト内の文字列を結合する場合も `str` 関数を利用できます：
```Clojure
(apply str ["Hello, " "world!"])
```
この場合も出力は "Hello, world!" となります。

## より深く知る

文字列の結合という概念は古くから存在し、これにはさまざまな方法とアプローチが存在します。Clojure では `str` 関数がこの役割を果たしていますが、他のプログラミング言語では、たとえば Java では `+` 演算子、Python では `join` 関数がそれにあたります。

また、`str` 関数はうまく最適化されており、特に大量の文字列を結合する場合は、結果の文字列が必要になるまで結合を遅延させることで、パフォーマンスの向上を達成しています。

## 参照リンク

さらなる学習のためのリンクをいくつか紹介します：

- Clojureの公式文書にある `str` 関数の解説 [http://clojuredocs.org/clojure.core/str]
- StackOverflow での `str` 関数に関する質疑応答 [https://stackoverflow.com/questions/5917959/whats-the-functional-way-of-joining-strings-in-clojure]
- Clojureでの文字列操作に関するブログ記事 [http://clojure-cookbook.com/recipes/strings]