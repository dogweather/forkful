---
title:                "Clojure: 文字列の結合"
simple_title:         "文字列の結合"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
文字列を連結することについて、なぜ誰もが関わるべきかを1-2文で説明します。

文字列の連結は非常に一般的な作業です。例えば、あなたがテキストメッセージを送るとき、複数の単語や句を一つのメッセージにまとめる必要があります。また、データベースのクエリを作成したい時なども、複数の文字列を結合する必要があります。そのような簡単な作業にも関わらず、文字列の連結を効率的に行う方法を知っておくことは重要です。

## 方法
```Clojure
(defn concat-strings [s1 s2]
  (str s1 s2))
(concat-strings "Hello" "World")

; output: HelloWorld
```
文字列を連結する方法はいくつかありますが、Clojureでは`str`関数を使うことができます。この関数は、与えられた文字列を一つの文字列にまとめ、新しい文字列を返します。上の例では、`concat-strings`という関数を定義し、渡された引数を`str`関数に渡しています。結果として、`HelloWorld`という単一の文字列が返されます。

## 深堀り
文字列を連結する方法としては、他にも`concat`や`join`という関数があります。`concat`関数は、複数のリストやベクターを結合することができます。一方、`join`関数は、特定のシンボルで区切られた文字列を結合することができます。これらの関数を使うことで、より複雑な文字列の連結を行うことができます。

## 参考文献
- [ClojureDocs - str](https://clojuredocs.org/clojure.core/str)
- [ClojureDocs - concat](https://clojuredocs.org/clojure.core/concat)
- [ClojureDocs - join](https://clojuredocs.org/clojure.string/join)