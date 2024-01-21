---
title:                "文字列の長さを求める"
date:                  2024-01-20T17:47:27.420371-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何とは？ & なぜ？)
文字列の長さを知るとは、その文字列が何文字から成っているかを数えることです。これはデータの整理、入力の検証、UIの設計などで役に立ちます。

## How to: (やり方)
Clojureでは、文字列の長さを得るのはシンプルです。

```clojure
(count "こんにちは") ; 5文字の文字列
;; => 5

(count "") ; 空の文字列
;; => 0

(count "Clojureは楽しい！")
;; => 11
```

サンプル出力は、コメントの後に示されます。

## Deep Dive (深掘り)
歴史的に見ると、文字列の長さを数える機能は多くの言語にとって基本です。Clojureでは`count`関数は文字列に対してだけでなく、リストやベクタなどのコレクションに対しても利用可能です。

他の方法として`length`や`size`といった関数がありますが、Clojureではこれらは存在しません。Clojureの`count`はJavaの`String.length()`メソッドを背後で使用していることを覚えておくと良いでしょう。

実装の詳細については、`count`はコレクションの実際の型に基づいて、最適な方法で長さを計算します。たとえば、文字列では直接Javaの`length()`が利用されますが、リストでは要素を一つずつ数えていきます。

## See Also (参照)
- Clojure公式ドキュメントの`count`関数: [https://clojuredocs.org/clojure.core/count](https://clojuredocs.org/clojure.core/count)
- Java平台のStringクラス: [https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#length()](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#length())