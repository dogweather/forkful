---
title:                "文字列の結合"
html_title:           "Clojure: 文字列の結合"
simple_title:         "文字列の結合"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何が& なぜ?

文字列を連結することは、プログラマーにとって非常に一般的な作業です。これは、複数の文字列を組み合わせて新しい文字列を作成することを意味します。例えば、"Hello"と"World"を連結すると、"Hello World"という新しい文字列ができます。プログラマーは、テキスト処理やデータのフォーマットなどのタスクを実行するために、文字列を連結することが必要になります。

## 方法:

```Clojure 
(str "Hello" " " "World") 
```
出力: "Hello World"

```Clojure 
(str "Welcome" ", " "John" "!")
```
出力: "Welcome, John!"

## 深く掘り下げる:

文字列の連結は、1950年代から存在する古典的なプログラミングタスクです。しかし、Clojureでは、文字列の連結に使用する関数が組み込まれています。しかし、文字列連結にはいくつかの異なる方法があります。一つは、JavaのStringBufferクラスを使用する方法で、もう一つは、Clojureのstr関数を使用する方法です。どちらの方法も、効率的な文字列連結を提供します。

## 関連記事:

- [Clojureドキュメント-文字列の連結](https://clojuredocs.org/clojure.core/str)
- [Javaバージョンの文字列連結の比較](https://stackoverflow.com/questions/2614022/string-concatenation-in-clojure-vs-java)