---
title:                "Clojure: テキストを検索および置換する"
simple_title:         "テキストを検索および置換する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

こんにちは、Clojureプログラムを学びたい皆さん！

## なぜ
テキストの検索と置換を行う理由は、データの整理や変換を簡単に行うことができるからです。

## 使い方
```Clojure
;; テキストの置換
(println (str/replace "Hello World!" "Hello" "こんにちは"))

;; 出力結果: こんにちは World!

;; 正規表現を使用した置換
(println (str/replace "Hello 123, World!" #"123" "こんにちは"))

;; 出力結果: Hello こんにちは, World!
```

## 深堀り
Clojureには、文字列を検索・置換するために使用できるいくつかの関数があります。`str/replace`を使用すると、テキスト内の特定の文字列を置換することができます。また、正規表現を使用して、より柔軟な置換を行うこともできます。

See Also

- [The Clojure Cookbook: Finding and Replacing](https://clojure-cookbook.com/strings/finding-and-replacing.html)
- [Clojure Dojo: String Replacement](https://clojuredocs.org/clojure.string/replace) 

それでは、今日からClojureでテキストの検索と置換を自在に行えるようになりましょう！