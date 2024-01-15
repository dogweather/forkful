---
title:                "ランダムな数値の生成"
html_title:           "Clojure: ランダムな数値の生成"
simple_title:         "ランダムな数値の生成"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
コンピュータープログラミングでランダムな数字を生成するのは、さまざまなアプリケーションやゲームで使用される重要な機能です。ランダムな値を生成することで、プログラムをより面白くしたり、多様性をもたらしたりすることができます。

## How To
```Clojure
;; ランダムな整数を生成する方法
(rand-int n)

;; ランダムな浮動小数点数を生成する方法
(rand)

;; 範囲内のランダムな整数を生成する方法
(rand-range start end)

;; ランダムな要素を取得する方法
(rand-nth coll)

;; 複数の値からランダムに1つ選択する方法
(rand-choice val1 val2 val3)

```

生成される乱数の範囲や種類は、引数の設定によって変更することができます。また、ランダムな値を使用する際には、必ず乱数の種を指定するようにしてください。

## Deep Dive
Clojureでは、乱数生成に様々なアルゴリズムを使用しています。その中でも、擬似乱数生成アルゴリズムは最も一般的な方法です。擬似乱数生成アルゴリズムでは、一定の計算に基づいて数値を生成し、ランダムに見える数字を取得することができます。

Clojureでは、デフォルトでJavaの擬似乱数生成アルゴリズムを使用していますが、安全性の観点から別のアルゴリズムを使用することもできます。その際には、`setRandomeSeed`関数を使用して乱数の種を設定することが必要です。

## See Also
- [Clojure ランダム - Clojure公式ドキュメント](https://clojure.org/reference/data_structures#hash-maps) 
- [Clojure Programming by Chas Emerick, Brian Carper, & Christophe Grand](https://www.amazon.co.jp/Clojure-Programming-Chas-Emerick/dp/1449394701/ref=sr_1_1?__mk_ja_JP=カタカナ&dchild=1&keywords=clojure&qid=1591477606&sr=8-1)