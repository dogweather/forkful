---
title:                "Clojure: 文字列の大小変換"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# なぜ「文字列のキャピタライズ」をするのか？

文字列のキャピタライズは、文字列から文書を作成する際に非常に便利です。例えば、名前やタイトルをそれぞれの単語の最初の文字だけ大文字にしたい場合や、データベースから取得した情報を見やすくするためなどに使用されます。

## 方法

Clojureでは、`capitalize`関数を使用して文字列をキャピタライズすることができます。以下のようなコードを書くことで、文字列をキャピタライズすることができます。

```Clojure
(capitalize "hello world")
```

このコードを実行すると、`"Hello world"`という結果が返されます。

また、大文字と小文字を区別しない場合は、`capitalize`の代わりに`capitalize*`を使用することができます。

```Clojure
(capitalize* "hello world")
```

この場合、`"Hello World"`という結果が返されます。

## 深堀り

Clojureでは、`clojure.string`ライブラリを使用することで、より複雑な文字列操作を行うことができます。`capitalize`関数を含むさまざまな関数が定義されており、それらを組み合わせることで、より柔軟な文字列処理を行うことができます。

例えば、全ての単語の先頭を大文字にしたい場合は、`clojure.string/capitalize-words`関数を使用することができます。

```Clojure
(clojure.string/capitalize-words "hello world")
```

この結果は、`"Hello World"`となります。

## もっと知りたい方は以下を参考にしてください

- [Clojureドキュメント - String functions](http://clojuredocs.org/clojure.string/capitalize)
- [Clojure for the Brave and True - Manipulating Strings](https://www.braveclojure.com/strings/)

# 関連記事

- [Clojure 101 – Introduction to Clojure Programming](http://www.codedependents.com/2013/10/21/clojure-101-introduction-clojure-programming/)