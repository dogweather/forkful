---
title:                "Clojure: 文字列の長さを見つける"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

誰もがプログラミングに携わったことがあるかもしれませんが、文字列の長さを見つけることは非常によく使われるタスクです。文字列の長さを見つけることは、文字列操作や検証に必要な重要なスキルです。

## How To

今回はClojureを使って文字列の長さを見つける方法を紹介します。まずは文字列を定義してみましょう。

```Clojure
(def str "こんにちは、世界！")
```

これで、文字列の長さを取得する準備ができました。次のコードを使って、文字列の長さを見つけることができます。

```Clojure
(count str)
```

これで、文字列「こんにちは、世界！」の長さである12が返されます。

## Deep Dive

Clojureでは、文字列の長さを見つけるために、主に組み込み関数であるcountを使います。この関数は、文字数や要素数を返します。また、文字列以外のコレクションやシーケンスに対しても、同様の動作をします。

しかし、注意点としては、count関数は文字列内の1文字を1とカウントするため、マルチバイト文字を含む文字列の場合は、正確な文字数を返すことができません。そのため、マルチバイト文字を含む文字列の場合は、文字列を正規化してからcount関数を使用する必要があります。

## See Also

- [Clojure Documentation on count function](https://clojuredocs.org/clojure.core/count)
- [Clojure中級者への道: 文字列を触る](https://qiita.com/angel_p_57/items/76eadedbae8fa77ca0bc)