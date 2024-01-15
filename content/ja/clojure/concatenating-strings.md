---
title:                "文字列の連結"
html_title:           "Clojure: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

##なぜ

Clojureでは、文字列を連結することは非常に一般的なタスクです。例えば、ユーザーからの入力を処理する際に、文字列を連結して最終的な結果を表示する必要があるかもしれません。また、データベースから取得した文字列を結合して、処理した結果を表示することもできます。文字列を連結することは、多くの場面で非常に便利な方法です。

##方法

文字列を連結するには、`str`関数を使用します。この関数は、引数として渡された文字列を順番に連結し、1つの文字列として返します。以下の例では、"Hello"と"world"という2つの文字列が連結されて、"Hello world"という出力が得られます。

```Clojure
(str "Hello" "world")
```

出力:
```
"Hello world"
```

また、カンマ区切りのリスト`clojure.string/join`を使用することで、リスト内の文字列を連結することもできます。以下の例では、"Hello"と"world"という2つの文字列がリスト内にあり、それらがカンマで結合されて1つの文字列として返されます。

```Clojure
(require '[clojure.string :as str])

(str/join "," ["Hello" "world"])
```

出力:
```
"Hello,world"
```

##深堀り

Clojureでは、文字列の連結には`str`関数以外にもいくつかの方法があります。例えば、`format`関数を使用することで、文字列の中に変数を埋め込んで出力することができます。

```Clojure
(require '[clojure.string :as str])

(def name "John")
(def age 30)

(str/format "My name is %s and I am %d years old." name age)
```

出力:
```
"My name is John and I am 30 years old."
```

また、Clojureでは文字列を連結する際に、メモリを無駄にすることなく効率的に処理するための最適化が行われています。これにより、大量の文字列を連結する場合でも、高速な処理が可能です。

##参照

- [Clojure - concat](https://clojuredocs.org/clojure.core/concat)
- [Clojure - format](https://clojuredocs.org/clojure.core/format)
- [Clojure - string](https://clojuredocs.org/clojure.string)