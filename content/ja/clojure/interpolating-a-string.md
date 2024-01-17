---
title:                "文字列の補間"
html_title:           "Clojure: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

# 何&なぜ？

文字列補間とは、文字列の中に変数や式を組み込むことを指します。プログラマーは、ダイナミックな文字列を生成するために文字列補間を使用します。

## 方法：

文字列補間を行うには、バッククォートとドル記号を使用します。例えば、変数`name`を含む文字列を作成する場合、` `Clojure (str ` `` ` ` ` ` ` ` ` ` $name` ` `) ` ` ` ` ` ` ` ` ` ` ` ` ` ` ` ` `というコードを使用します。このように、バッククォートで囲んだ文字列内のドル記号に続けて変数や式を置くことで、文字列内に値を埋め込むことができます。

以下は、Clojureで文字列補間を行う例です。

```
Clojure (str ` ` "Hello, " $name "! How are you today?") ` ` 
```

このコードは、文字列`Hello, [変数nameの値]! How are you today?`を生成します。

## ディープダイブ：

Clojureには、文字列補間に使用できる他の方法もあります。例えば、`format`関数を使用することもできます。また、`StringBuilder`を使用して文字列を連結する方法もあります。しかし、Clojureではバッククォートとドル記号を使用することで、より簡潔かつ効率的に文字列補間を行うことができます。

## 関連サイト：

- [Clojureチュートリアル](https://clojure.org/guides/getting_started)

- [Clojure公式ドキュメント](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/format)

- [Clojureの文字列補間の仕組みについて](https://www.braveclojure.com/core-library/#Format_and_String_interpolation)