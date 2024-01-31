---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"

category:             "Clojure"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の大文字化とは、その文字列の中の文字を大文字に変換することです。プログラマーはデータの一貫性を保つためや、ユーザーインターフェイスの表示を改善するためにこれを行います。

## How to: (方法)
```Clojure
;; `clojure.string` ライブラリを使って文字列を大文字に変換する
(require '[clojure.string :as str])

;; 文字列 "hello world" を大文字に変換
(str/capitalize "hello world")
;; 出力: "Hello world"

;; 文字列全体を大文字に変換
(str/upper-case "hello world")
;; 出力: "HELLO WORLD"
```

## Deep Dive (深掘り)
Clojureでの文字列大文字化は、主に `clojure.string` ライブラリを使用し実行されます。このライブラリはClojureコアリーブラリの中に存在し、多くの標準的な文字列操作を提供します。

過去には、Javaのメソッドを直接Clojureから呼び出すことで大文字化を実現することもありました（例：`.toUpperCase`）。これは、ClojureがJVMの上に構築された言語であるためです。ただし、`clojure.string` ライブラリの関数を使用する方がClojureらしい方法です。

`str/capitalize` 関数と `str/upper-case` 関数は微妙に異なります。`str/capitalize` は最初の文字のみを大文字にし、残りの文字はそのままにするのに対し、`str/upper-case` は文字列内の全ての文字を大文字に変換します。状況に応じて適切な関数を選択することが重要です。

## See Also (参照)
- Clojureの公式ドキュメント: [clojure.string API](https://clojure.github.io/clojure/clojure.string-api.html)
- JavaとClojureの比較:[ClojureからJavaを呼び出す](https://clojure.org/reference/java_interop)
- より深い文字列操作のためのライブラリ: [clojure.string](https://github.com/clojure/clojure/blob/master/src/clj/clojure/string.clj)
