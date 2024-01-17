---
title:                "テキストの検索と置換"
html_title:           "Clojure: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なにだ? どうして?

テキストを検索・置換するとは、プログラマーがコード内の特定のテキストを見つけて、それを別のテキストに置き換えることです。プログラマーは、コード内の特定のテキストを効率的に変更するために、検索と置換を行います。

## 使い方:

```Clojure
; 例1: インデックスの置換
(clojure.string/replace "Hello, World!" "," " ")
> "Hello World!"

; 例2: 正規表現を使用した複雑な置換
(clojure.string/replace "Hello, World!" #"[^\w\s]" "_")
> "Hello__World_"
```

## 詳しい情報

検索と置換は、コードの編集やデータの処理において一般的なテクニックです。歴史的には、テキストエディターで使用される正規表現が広く使われてきました。また、Clojureには、検索・置換のための多様な関数やライブラリがあります。

## 関連情報

- [正規表現についての Clojure ドキュメント](https://clojure.org/guides/learn/functions#_regular_expressions)
- [Clojure における文字列操作についてのガイド](https://clojure.org/guides/learn/functions#_string_manipulation)