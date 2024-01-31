---
title:                "正規表現の使用"
date:                  2024-01-19
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"

category:             "Clojure"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? - 正規表現とは？なぜ使う？

正規表現は文字列のパターンマッチングに使います。シンプルかつ効率的に文字列を検索・置換するためにプログラマーが利用しています。

## How to: - 使い方

```Clojure
;; 文字列にマッチさせる
(re-seq #"\b[Cc]lojure\b" "Clojure has clojure and more CLOJURE")
;; => ("Clojure" "clojure")

;; 文字列を置換する
(clojure.string/replace "Learning Clojure programming" #"\b[Cc]lojure\b" "LISP")
;; => "Learning LISP programming"

;; 文字列を分割する
(clojure.string/split "a,b,c,d" #",")
;; => ["a" "b" "c" "d"]
```

## Deep Dive - 詳細情報

正規表現は、1960年代に発展し、多くのプログラミング言語に組み込まれています。Clojureでは`java.util.regex`パッケージを内部的に利用しています。`re-find`, `re-seq`, `re-matches`などの関数が提供されており、柔軟に正規表現を扱うことができます。Clojureにおける代替手段としては、文字列関数やパーサーコンビネータがありますが、正規表現は一般に最も高速です。

## See Also - 参照

- Clojureの公式ドキュメント: [clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)
- Javaの `Pattern` クラス: [java.util.regex.Pattern](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
