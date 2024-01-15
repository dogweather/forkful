---
title:                "文字列の抽出"
html_title:           "Clojure: 文字列の抽出"
simple_title:         "文字列の抽出"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

もし誰かが文字列を部分文字列に分割することに興味があるのならば、この記事はあなたにとって役に立つでしょう。

## 方法

部分文字列を抽出するには、Clojureの[subs](https://clojuredocs.org/clojure.core/subs)関数を使用します。

```Clojure
(subs "Hello world" 0 5)
```

このコードでは、文字列"Hello world"の先頭から5文字目までの部分文字列が抽出されます。出力は以下のようになります。

```Clojure
"Hello"
```

部分文字列を抽出する際、取り出したい文字列の範囲を指定することが重要です。上記の例では、始点と終点のインデックスを指定しましたが、指定しない場合は文字列の先頭から終端までの部分文字列が抽出されます。

```Clojure
(subs "Hello world" 3)
```

このコードでは、文字列の先頭から3番目の文字列から終端までの部分文字列が抽出されます。出力は以下のようになります。

```Clojure
"lo world"
```

文字列の範囲を指定する際、負のインデックスを使用することもできます。負のインデックスを指定すると、文字列の末尾から数えて範囲を指定できます。

```Clojure
(subs "Hello world" -2)
```

このコードでは、文字列の末尾から2番目の文字列から終端までの部分文字列が抽出されます。出力は以下のようになります。

```Clojure
"ld"
```

## ディープダイブ

Clojureの[subs](https://clojuredocs.org/clojure.core/subs)関数は再帰的な部分文字列の抽出もサポートしています。つまり、抽出した部分文字列から更に部分文字列を抽出することができます。

```Clojure
(subs (subs "Hello world" 0 5) 1)
```

このコードでは、文字列全体から5文字目までの部分文字列を抽出し、その中から2番目の文字から終端までの部分文字列が抽出されます。出力は以下のようになります。

```Clojure
"ello"
```

文字列の範囲を指定する際、始点より終点のインデックスの方が小さい場合、空の文字列が返されます。

```Clojure
(subs "Hello world" 3 0)
```

このコードでは、文字列の3番目の文字から、文字列の0番目の文字までの部分文字列が抽出されます。しかし、文字列の3番目の文字より前に0番目の文字があるわけがないので、結果は空の文字列になります。出力は以下のようになります。

```Clojure
""
```

## もっと詳しく知りたい方は

- [ClojureDocs: subs](https://clojuredocs.org/clojure.core/subs)
- [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/): 参考としておすすめの本です。
- [Clojure Koans](https://github.com/functional-koans/clojure-koans): Clojureの基本を学べる練習問題です。

***

## 関連記事

- [Clojureで文字列を連結する方法](https://example.com/clojure-string-concatenation)