---
title:    "Clojure: 小文字に文字列を変換する"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換することのメリットは、同じ文字列が異なる大文字と小文字の組み合わせで表されることがあるため、一貫性のないデータを整理するのに役立ちます。

## 方法
```Clojure
; 文字列を格納する変数を定義する
(def my-string "Hello World")

; 文字列を小文字に変換する
(.toLowerCase my-string)

; 変換後の結果を確認する
(print "小文字に変換後の文字列: " (.toLowerCase my-string))
```

上記のコードを実行すると、`小文字に変換後の文字列: hello world`という結果が得られます。変換前の文字列に大文字が含まれていた場合でも、すべて小文字に変換されます。

## ディープダイブ
Clojureでは、文字列を小文字に変換するには`(.toLowerCase string)`メソッドを使います。このメソッドはJavaの`toLowerCase()`と同様の機能を持っており、文字列を小文字に変換します。ただし、Clojureでは文字列を操作するための独自の関数やマクロを提供しているため、これらを使っても同じ結果を得ることができます。

また、Clojureでは文字列の大文字と小文字を無視するための比較関数も提供されています。例えば、`(= "hello" "Hello")`という式は`true`を返します。

## See Also
- [Clojure 文字列操作関数](https://clojure.org/api/cheatsheet#String Functions)
- [文字列の大文字と小文字を無視する](https://clojure.org/guides/weird_characters#_ignoring_case)