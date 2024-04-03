---
date: 2024-01-20 17:34:38.190953-07:00
description: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u8907\u6570\u306E\
  \u6587\u5B57\u5217\u3092\u3064\u306A\u304E\u5408\u308F\u305B\u3066\u4E00\u3064\u306E\
  \u6587\u5B57\u5217\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u3092\u307E\u3068\u3081\u305F\u308A\u3001\
  \u30E6\u30FC\u30B6\u30FC\u5411\u3051\u306E\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u4F5C\
  \u6210\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.546584-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\u8907\u6570\u306E\
  \u6587\u5B57\u5217\u3092\u3064\u306A\u304E\u5408\u308F\u305B\u3066\u4E00\u3064\u306E\
  \u6587\u5B57\u5217\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u3092\u307E\u3068\u3081\u305F\u308A\u3001\
  \u30E6\u30FC\u30B6\u30FC\u5411\u3051\u306E\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u4F5C\
  \u6210\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## How to: (方法)
Clojureで文字列を連結するには、`str`関数を使うのが一般的です。ここではシンプルな例をいくつか紹介します。

```clojure
;; 単純な文字列の連結
(str "こんにちは、" "Clojure!")

;; 変数を含む文字列の連結
(def name "世界")
(str "こんにちは、" name "!")

;; 数値と文字列の連結
(str "答えは" 42 "です。")

;; リスト内の文字列を連結
(str/join ", " ["リンゴ" "バナナ" "チェリー"])
```

サンプル出力:
```
"こんにちは、Clojure!"
"こんにちは、世界!"
"答えは42です。"
"リンゴ, バナナ, チェリー"
```

## Deep Dive (詳細)
文字列の連結をするためにClojureでは`str`関数が用いられますが、これは多くの異なる型のデータを受け取り、文字列に変換して連結します。Clojureがこの関数を追加した背景には、Lisp言語の影響があります。

代替手段として、`format`関数や`clojure.string/join`関数がありますが、状況に応じて適した方法を選ぶ必要があります。

`str`は内部的にJavaのStringBuilderクラスを使用しており、大量の文字列を連結する場合にも効率的です。

## See Also (関連情報)
- Clojure公式ドキュメントの`str`関数: [https://clojuredocs.org/clojure.core/str](https://clojuredocs.org/clojure.core/str)
- JavaのStringBuilderクラスに関する詳細: [https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- `format`関数の使い方: [https://clojuredocs.org/clojure.core/format](https://clojuredocs.org/clojure.core/format)
- `clojure.string/join`関数: [https://clojuredocs.org/clojure.string/join](https://clojuredocs.org/clojure.string/join)
