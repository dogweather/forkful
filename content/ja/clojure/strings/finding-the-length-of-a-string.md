---
date: 2024-01-20 17:47:27.420371-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u77E5\u308B\u3068\u306F\u3001\
  \u305D\u306E\u6587\u5B57\u5217\u304C\u4F55\u6587\u5B57\u304B\u3089\u6210\u3063\u3066\
  \u3044\u308B\u304B\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\
  \u306F\u30C7\u30FC\u30BF\u306E\u6574\u7406\u3001\u5165\u529B\u306E\u691C\u8A3C\u3001\
  UI\u306E\u8A2D\u8A08\u306A\u3069\u3067\u5F79\u306B\u7ACB\u3061\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.545435-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u77E5\u308B\u3068\u306F\u3001\
  \u305D\u306E\u6587\u5B57\u5217\u304C\u4F55\u6587\u5B57\u304B\u3089\u6210\u3063\u3066\
  \u3044\u308B\u304B\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\
  \u306F\u30C7\u30FC\u30BF\u306E\u6574\u7406\u3001\u5165\u529B\u306E\u691C\u8A3C\u3001\
  UI\u306E\u8A2D\u8A08\u306A\u3069\u3067\u5F79\u306B\u7ACB\u3061\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
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
