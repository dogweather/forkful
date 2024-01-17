---
title:                "ランダムな数字を生成する"
html_title:           "Clojure: ランダムな数字を生成する"
simple_title:         "ランダムな数字を生成する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何&何故？
ランダムな数字を生成するとは何か、プログラマーがそれをする理由とは何か。
ランダムな数字を生成することは、プログラミングにおいて重要な役割を果たします。例えば、ゲームではランダムな出現物を制御するために使用されます。プログラマーがこれを行う理由は、プログラムをよりダイナミックにし、予測不可能性を与えることによって、より面白くするためです。

## 使い方：
```Clojure
;; ランダムな整数を生成する
(rand-int 100) ;=> 57
(rand-int 100) ;=> 18
(rand-int 100) ;=> 95

;; ランダムな小数を生成する
(rand) ;=> 0.3811766173551662
(rand) ;=> 0.6314673090387878
(rand) ;=> 0.8876914318785585

;; 特定の範囲内のランダムな小数を生成する
(rand 10) ;=> 6.279056110577476
(rand 10) ;=> 2.782083457070899
(rand 10) ;=> 9.643528608096088
```

## 深い掘り下げ：
ランダムな数字を生成するためには、コンピューターには様々なアルゴリズムが使用されています。一つは線形合同法と呼ばれるもので、ある種のランダム性を持った乱数列を生成します。Clojureでは、`rand-int`関数や`rand`関数を使用することで簡単にランダムな数字を生成することができます。しかし、より高度なランダム性が求められる場合は、より複雑なアルゴリズムを実装する必要があるかもしれません。

## 参考情報：
- [Clojure公式ドキュメント](https://clojure.org/reference/numbers#_random_numbers)
- [ランダム数生成器の種類と特徴](https://future-creator.github.io/2018/11/30/developer-note-about-random/)