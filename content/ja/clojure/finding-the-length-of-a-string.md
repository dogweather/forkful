---
title:                "文字列の長さを見つける"
html_title:           "Clojure: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることについて、なぜそのことに注目する必要があるのかの最大２つの文を説明します。

文字列の長さを求めることは、プログラミングにおいて非常に一般的なタスクです。例えば、ユーザーが入力した文字列の長さを確認するために使用されることがあります。

## 方法

文字列の長さを求めるには、Clojureの`count`関数を使用します。この関数は文字列の長さを返すために使用することができます。

```Clojure
(count "Hello")
```

このコードを実行すると、コンソールには「5」という値が表示されます。これは、文字列「Hello」の長さが5文字であることを示しています。

さらに、文字列リストの長さを求めることもできます。

```Clojure
(count ["apple" "orange" "banana"])
```

このコードを実行すると、コンソールには「3」という値が表示されます。これは、文字列リストに含まれる要素の数が3つであることを示しています。

## 深堀り

Clojureの`count`関数は、単純に文字列やリストの要素の数を返すだけでなく、さまざまなデータ構造に対しても動作します。文字列やリストの長さを求めるだけでなく、マップやセットの要素の数を調べることもできます。

また、`count`関数はClojureの`seq`パラメータを受け取り、シーケンス型のデータ構造に対しても動作するように拡張することができます。さらに、カスタム`count`関数を作成することで、独自のデータ構造に対しても長さを求めることができます。

## 関連リンク

- [Clojure 公式ドキュメント](https://clojure.org/)
- [Clojure Tutorial](https://www.johnmurch.com/2020/07/clojure.html)
- [Clojure文法覚書](https://qiita.com/campania21/items/5b163cdeb258800b0ce0)