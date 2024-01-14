---
title:    "Clojure: ランダムの数字を生成する"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

乱数を生成することに興味がある人にとって、なぜこのようなことを行うのかは重要な質問です。乱数は、ゲームや暗号化、統計学など様々な分野で使用される重要なツールです。

## 方法

乱数を生成する方法は多くありますが、Clojureを使用することで簡単かつ効率的に実装することができます。以下のコードブロックは、Clojureで乱数を生成する方法を示しています。

```Clojure
; 1から10までの間の整数乱数を生成する
(rand-int 10)

; 0から1までの間の小数乱数を生成する
(rand)

; シード値を指定して乱数を生成する
(random-seed 1234)
(rand-int 100)
```

上記のように、`rand-int`と`rand`の2つの関数を使用して、整数や小数の乱数を生成することができます。また、`random-seed`関数を使用することで、同じシード値を指定することで同じ乱数を再現することができます。

## ディープダイブ

乱数を生成するアルゴリズムには様々なものがありますが、ClojureではMersenne Twisterアルゴリズムが使用されています。このアルゴリズムは高速かつ疑似乱数を生成することができるため、広く使われています。

さらに、Clojureでは乱数の生成にJavaの`java.util.Random`クラスも使用されています。このクラスはMersenne Twisterアルゴリズムの改良版であり、より高品質な乱数を生成することができます。

## 詳細情報

- [Clojure公式ドキュメント](https://clojuredocs.org/clojure.core/rand-int)
- [乱数生成アルゴリズムの比較](https://qiita.com/nkoketsu/items/77cfdabb961f1eb2a8ab)
- [Mersenne Twisterアルゴリズムの説明](https://en.wikipedia.org/wiki/Mersenne_Twister)

## 参考

[See Also]
- [Javaのjava.util.Randomクラスの詳細情報](https://docs.oracle.com/javase/7/docs/api/java/util/Random.html)
- [Clojureで乱数を生成する方法](https://tech-ojisan.com/random-number-clojure/)