---
date: 2024-01-20 17:47:27.420371-07:00
description: "How to: (\u3084\u308A\u65B9) Clojure\u3067\u306F\u3001\u6587\u5B57\u5217\
  \u306E\u9577\u3055\u3092\u5F97\u308B\u306E\u306F\u30B7\u30F3\u30D7\u30EB\u3067\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.494108-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Clojure\u3067\u306F\u3001\u6587\u5B57\u5217\u306E\u9577\
  \u3055\u3092\u5F97\u308B\u306E\u306F\u30B7\u30F3\u30D7\u30EB\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
