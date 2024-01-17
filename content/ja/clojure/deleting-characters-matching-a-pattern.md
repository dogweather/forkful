---
title:                "パターンに一致する文字を削除する"
html_title:           "Clojure: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Clojureに興味を持ってくれてありがとう！ここでは、特定のパターンと一致する文字を削除するClojureの機能について紹介していきたいと思います。この機能は一見あまり重要ではないように思えますが、実はプログラマーにとって非常に役に立つものです。

## What & Why?

この機能は、文字列内の特定のパターンと一致する文字を削除するものです。例えば、改行やタブ、空白などが該当します。プログラマーがこの機能を使う理由は、文字列の整形や処理をする際に不要な文字を削除することで、より効率的にコードを書くことができるからです。

## How to:

```
; ここでは、"Hello, World!"という文字列からカンマとスペースを削除する例を示します。
; 文字列を定義します。
(def str "Hello, World!")
; カンマとスペースを指定して削除します。
(clojure.string/replace str #", " "")
; 出力は"HelloWorld!"になります。
```

```
; もう一つの例として、文字列内の数字を削除する方法を紹介します。
; 文字列を定義します。
(def str "Apple1234Orange5678Banana")
; 数字を指定して削除します。
(clojure.string/replace str #"[0-9]" "")
; 出力は"AppleOrangeBanana"になります。
```

## Deep Dive:

この機能は、Clojure 1.2以降で利用可能になりました。以前は、正規表現を使って同様のことを行う必要がありましたが、よりシンプルに文字列を処理することができるようになったのです。なお、文字列を置換する場合は、`replace`関数の代わりに`str`関数を使うこともできます。

また、文字列ではなくシーケンスを処理する際は、`clojure.core/remove`関数を使用することで同様の結果を得ることができます。

## See Also:

- [Clojure String API](https://clojure.org/reference/java_interop#_clojure_string_api)
- [Clojure Cheatsheet](https://clojure.org/api/cheatsheet)