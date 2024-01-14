---
title:                "Clojure: 文字列の連結"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

##なぜ 
文字列をつなぎ合わせるのかについての理由は、より複雑なデータを作るためです。たとえば、ユーザーの名前と姓を組み合わせてフルネームを作成したり、文章を動的に変更するために必要となる場合があります。

##方法
文字列をつなぎ合わせるには、Clojureの **str** 関数を使用します。この関数に結合したい文字列を引数として渡すことで、簡単に文字列を結合することができます。

```Clojure
(str "こんにちは" "、私の名前は" "太郎です。") 
```

このコードを実行すると、以下のような出力が得られます。

```
こんにちは、私の名前は太郎です。
```

##深堀り
Clojureでは、 **str** 関数以外にも、 **interpose** や **clojure.string/join** 等の関数を使用して文字列を結合することができます。これらの関数を使うことで、様々な方法で文字列を結合することができます。

また、文字列の中に変数の値を埋め込む方法として、 **format** 関数を使用することもできます。この関数を使うと、より柔軟に文字列を結合することができます。

##見てみる
- [Clojure入門: 文字列操作](https://www.scalamatsuri.org/ja/2014/schedule/presentation/10/)
- [文字列の結合 (Clojure)](https://docs.microsoft.com/ja-jp/learn/modules/clojure-string-concatenation/) 
- [Clojureで文字列を操作する方法](https://dotinstall.com/lessons/basic_clojure_v3/24555)

##関連リンク
- [Clojure公式ドキュメント](https://clojuredocs.org/)
- [Clojureチュートリアル](https://clojure.org/guides/getting_started)
- [Clojure入門講座](https://www.udemy.com/course/clojure-programming-beginners/)