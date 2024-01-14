---
title:                "Clojure: パターンに一致する文字を削除する"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ
誰かがパターンにマッチする文字を削除することについて取り組む必要があるのか、その理由を簡単に説明します。

## やり方
コーディングの例とサンプルの出力を、 ```Clojure ... ```というコードブロックの中に記載します。

パターンにマッチする文字を削除する方法は簡単です。まず、<code>clojure.string/replace</code>関数を使って文字列からパターンにマッチする部分を削除します。

<code>clojure.string/replace</code>関数の使用例を以下に示します。パターンとして```"a"```を指定すると、文字列から"a"が削除されます。サンプルの出力では、文字列から"a"が削除されたことが確認できます。

 ```Clojure
(clojure.string/replace "banana" "a" "")
;; => "bnn"
```

これに加えて、正規表現を使うことでより複雑なパターンにもマッチすることができます。例えば、```"b[aeiou]n"```というパターンを使うと、文字列から"ban"や"ben"などの部分が削除されます。

 ```Clojure
(clojure.string/replace "banana" #"b[aeiou]n" "")
;; => "ana"
```

## 詳細な説明
パターンにマッチする文字を削除する方法について、さらに詳細に説明します。

まず、Clojureでは文字列を表すデータ型として```java.lang.String```クラスを使用します。このクラスには、文字列の操作を行うための多くのメソッドが用意されています。

Clojureでは文字列をハッシュマップと見なすことができ、そのキーと値を使って文字列の操作を行うことができます。例えば、```"banana"```という文字列をハッシュマップと見なした場合、以下のように表現することができます。

 ```Clojure
{:b 2, :a 3, :n 2}
```

このように、文字列が実際にはキーと値の組み合わせで表現されているため、ハッシュマップの操作を利用してパターンにマッチする文字を削除することができます。

## 参考文献
- [Clojure入門](https://www.clojure.or.jp/gokaiban/clojure_intro.html)
- [Clojureでの文字列操作](https://qiita.com/tadsan/items/ed2d36e3964e337aaa9a)

## 関連リンク
- [Clojureリファレンス](https://clojure.github.io/clojure/clojure.core-api.html)
- [正規表現を使った文字列の置換](https://qiita.com/shintakuya/items/9b0014b56fad1c9ed09f)