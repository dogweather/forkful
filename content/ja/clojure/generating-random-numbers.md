---
title:                "Clojure: ランダム数字の生成"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数字の生成に参加する理由は何でしょうか？数字をランダムに生成することで、様々なアプリケーションやゲームをより興味深くしたり、データの偏りを防ぐことができるからです。

## 方法

Clojureでランダムな数字を生成する方法は簡単です。以下のように、 ```Clojure (rand) ```関数を使うことで、乱数を生成することができます。

```Clojure
(rand) ;;=> 0.567208421482615
```

また、特定の範囲内の乱数を生成するには、 ```Clojure (rand-int n) ```関数を使うことができます。 ```n``` は生成する最大の整数値を指定します。

```Clojure
(rand-int 100) ;;=> 47
(rand-int 5) ;;=> 3
```

さらに、シード値を指定することで、同じ乱数を生成することもできます。

```Clojure
(def r (random-seed 1234))
(rand r) ;;=> 0.8586351428435014
(rand r) ;;=> 0.8586351428435014 (同じ値が出力される)
```

## ディープダイブ

Clojureでは、乱数を生成するためのさまざまな関数が用意されています。例えば、 ```rand-nth ```関数を使うことで、リストやベクターからランダムな要素を選択することができます。

```Clojure
(rand-nth ["apple", "banana", "orange"]) ;;=> "banana"
(rand-nth [1 2 3 4 5]) ;;=> 3
```

また、2つの値の間の乱数を生成するには、 ```rand-range ```関数を使うことができます。 ```Clojure (rand-range min max) ```のように使います。

```Clojure
(rand-range 1 10) ;;=> 7
(rand-range 100 200) ;;=> 145
```

さらに、乱数の分布を指定することで、特定のパターンの数値を生成することもできます。例えば、正規分布を指定することで、平均値や標準偏差に基づいた数値を生成することができます。

```Clojure
(def normal (normal-distribution 50 10)) ;; 平均値: 50, 標準偏差: 10
(rand normal) ;;=> 48.74567258147989
```

## 関連リンク

- [Clojure ドキュメンテーション](https://clojure.org/)
- [Clojure の乱数生成についてのドキュメンテーション](https://clojuredocs.org/clojure.core/rand)
- [Clojureの乱数生成についてのチュートリアル](https://www.braveclojure.com/core-functions-in-depth/#Random)
- [Clojureの乱数生成についてのブログ記事](https://purelyfunctional.tv/article/generate-random-values-clojure/)