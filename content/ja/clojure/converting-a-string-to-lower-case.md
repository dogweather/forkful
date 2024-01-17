---
title:                "文字列を小文字に変換する"
html_title:           "Clojure: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何をして、なぜ？
文字列を小文字に変換することは、単純に文字列のすべての文字を小文字に変換することです。プログラマーは、大文字と小文字を区別しない比較を行いたい場合や、文字列の外観を統一したい場合など、さまざまな理由でこれを行います。

## 方法：
以下のコード例を使用して、文字列を小文字に変換する方法を示します。```Clojure
(.toLowerCase "Hello World")
;;=> "hello world"

(.toLowerCase "APPLE")
;;=> "apple"
``` 

## 深く掘り下げる：
- 歴史的な文脈：文字列を大文字と小文字に分ける方法は、意外と難しい問題でした。コンピューター上で大文字と小文字を区別するのは、印刷機械の登場以前の古い印刷方法に遡ることができます。
- 代替案：他のプログラミング言語では、文字列を小文字に変換するために専用のメソッドが利用可能です。Clojureでは、Javaのメソッドを使用することで文字列を小文字に変換することができます。
- 実装の詳細：Clojureでは、Javaの文字列クラスのメソッド `.toLowerCase` を使用して、文字列を小文字に変換しています。

## 関連情報へのリンク：
- [Clojureドキュメント - `.toLowerCase`](https://clojuredocs.org/clojure.core/to-lower-case)
- [Java文字列クラス - `.toLowerCase`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)