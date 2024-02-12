---
title:                "文字列の連結"
aliases: - /ja/clojure/concatenating-strings.md
date:                  2024-01-20T17:34:38.190953-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の連結とは、複数の文字列をつなぎ合わせて一つの文字列にすることです。プログラマーはデータをまとめたり、ユーザー向けのメッセージを作成したりするためにこれを行います。

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
