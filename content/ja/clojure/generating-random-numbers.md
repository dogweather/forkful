---
title:                "Clojure: ランダム数の生成"
simple_title:         "ランダム数の生成"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ: ランダムな数値の生成を行う必要性

ランダムな数値の生成にはさまざまな理由があります。たとえば、ゲームやシミュレーション、暗号化、テストデータの作成などが挙げられます。また、ランダム性を持たせることでプログラムの予想外の動作を確認することもできます。

## 方法: コード例とサンプル出力

```Clojure
; ランダムな整数の生成 (1から10まで)
(rand-int 10) ; => 7

; ランダムな小数の生成 (0から1まで)
(rand) ; => 0.546928458

; 指定した範囲内のランダムな数値の生成
(rand-nth (range 20 30)) ; => 24

; ランダムな文字列の生成 (A-Zから10文字)
(apply str (repeatedly 10 #(char (+ (rand-int 26) 65)))) ; => "REFDZBNRHJ"

```

## ディープダイブ: ランダムな数値生成について詳しく

Clojureでは、ランダムな数値を生成するために`rand`や`rand-int`などの関数が利用できます。これらの関数は内部的にはPseudo-random number generator (PRNG)を使用しており、シード値を指定することで再現性を持たせることもできます。

また、`rand`関数はデフォルトでは0から1の範囲内の小数を返しますが、`rand-int`は整数のみを返します。さらに、`range`や`repeatedly`などのClojureの他の関数を組み合わせることで、さまざまなランダムな数値の生成が可能です。

## 参考リンク

- [Clojureのドキュメンテーション](https://clojuredocs.org/clojure.core/rand)
- [Clojureでランダムな数字を生成する方法](https://qiita.com/yuya_takeyama/items/5e91170c247618eb3a02)
- [Understanding random numbers in Clojure](https://blog.8thlight.com/axis-of-entropy/2011/01/15/understanding-random-numbers-in-clojure.html)

## 参考書籍

- 『Clojureプログラミングクックブック』, Luke VanderHart, Ryan Neufeld
- 『プログラミングClojure 第2版』, Stuart Halloway
- 『Mastering Clojure』, Ed Ipri, Akhil Wali