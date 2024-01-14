---
title:    "Clojure: 「文字列を小文字に変換する」"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することのメリットは何ですか？Clojureプログラミングをする上で、なぜ文字列を小文字に変換する必要があるのでしょうか？

## 方法

Clojureでは、文字列を小文字に変換するための便利な関数があります。下記のコード例を参考にしてください。

```Clojure
; 文字列を小文字に変換
(.toLowerCase "HELLO WORLD") ; 出力結果：hello world

; 変数に代入して使うこともできます
(def greeting "こんにちは")
(.toLowerCase greeting) ; 出力結果：こんにちは
```

このように、`.toLowerCase`関数を使うことで、簡単に文字列を小文字に変換することができます。

## 深堀り

Clojureでは、`.toLowerCase`関数を使用する際に、文字の大きさを無視することができます。これは、文字の大小を区別する必要がない場合や、データの整形をする際にとても便利です。また、`.toUpperCase`関数を使うことで、文字列を大文字に変換することもできます。

さらに、`clojure.string`ライブラリを使用することで、より多くの文字列操作ができるようになります。例えば、`.replace`関数を使うことで、指定した文字列を別の文字列に置き換えることができます。

## 関連リンク

 [簡単に始めるClojure](https://clojure.org/guides/getting_started)

 [Clojureドキュメント](https://clojuredocs.org/)

 [clojure.stringライブラリのドキュメント](https://clojuredocs.org/clojure.string)