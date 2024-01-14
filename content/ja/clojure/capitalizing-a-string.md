---
title:    "Clojure: 文字列の大文字化"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# なぜ
文字列を大文字で表記するのに興味があるのはなぜでしょうか？私たちが慣れ親しんでいるテキストの一部を大文字で表すことで、より強調された印象を与えることができます。

## 方法
Clojureでは、 `clojure.string/capitalize`関数を使用することで、文字列を簡単に大文字にすることができます。例を見てみましょう。

```
; 文字列の宣言
(def str "hello world")
; 文字列を大文字にする
(clojure.string/capitalize str)

; 出力
"Hello world"
```

また、 `clojure.string/upper-case`関数を使用することで、すべての文字を大文字にすることもできます。

```
; 文字列の宣言
(def str "hello world")
; 全ての文字を大文字にする
(clojure.string/upper-case str)

; 出力
"HELLO WORLD"
```

## 深堀り
Clojureでは、文字列を大文字にする方法が複数用意されています。今回紹介した方法以外にも、 `clojure.string/-capitalize-first`関数を使用することで、最初の文字だけを大文字にすることもできます。

```
; 文字列の宣言
(def str "hello world")
; 最初の文字だけを大文字にする
(clojure.string/-capitalize-first str)

; 出力
"Hello world"
```

## 関連リンク
- [Clojure Documentation - String Functions](https://clojuredocs.org/clojure.string)
- [Clojure StringType Functions](https://www.tutorialspoint.com/clojure/clojure_string_functions.htm)
- [Clojure String Functions Examples](https://www.javatpoint.com/clojure-string-functions-examples)