---
title:                "正規表現を使用する"
html_title:           "Clojure: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何と、なぜ？
正規表現を使うとは、文字やパターンマッチングを簡単に行うことができるプログラミングの手法です。プログラマーがこれを行う理由は、特定の文字列を検索したり、変更したり、フィルタリングしたりすることが簡単になるからです。

## 使いかた：
```Clojure
;; 文字列の検索
(re-find #"apple" "I love apples!")
;; 結果： "apple"

;; パターンマッチング
(re-find #"\d{3}-\d{2}-\d{4}" "My social security number is 123-45-6789.")
;; 結果： "123-45-6789"

;; 文字列の変更
(clojure.string/replace "Hello World" #"World" "Universe")
;; 結果： "Hello Universe"
```

## 詳細について：
正規表現は、1960年代から使われている古い技術です。他のプログラミング言語でも利用できますが、Clojureでは特別な記法を使用して簡単に実装することができます。正規表現以外にも、文字列操作にはさまざまな方法がありますが、特定のパターンを検索したり、変更したりする際には正規表現が最適な選択肢です。Clojureでは、`re-matches`や`re-seq`のような便利な関数が正規表現と組み合わせて使うことができます。

## 関連リンク：
- [Official Clojure Guide on Regular Expressions](https://clojure.org/guides/regular_expressions)
- [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Learn X in Y minutes - Regular Expressions](https://learnxinyminutes.com/docs/regular-expressions/)