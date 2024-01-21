---
title:                "生成随机数"
date:                  2024-01-20T17:48:57.216498-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
生成随机数就是让计算机给我们一个看起来是纯碎随机的数字。程序员们这样做是因为随机数在很多情景中都很有用，比如游戏开发、测试和数据加密。

## How to (怎么做):
Clojure里生成随机数很简单。这里有几个例子：

```clojure
;; 引入随机数库
(require '[clojure.java.math :as math])

;; 生成0到1之间的随机浮点数
(random)

;; 生成一个随机整数
(rand-int 10)

;; 生成一个在0到100之间的随机浮点数
(* (random) 100)

;; 用不同的随机种子（时间戳）生成一个随机数
(math/random-seed (System/currentTimeMillis))
(random)
```

样例输出：
```clojure
0.7158279089358349  ; 随机浮点数
7                   ; 0到9之间的随机整数
85.58243234033394   ; 0到100之间的随机浮点数
0.4759706883430481  ; 有随机种子的随机浮点数
```

## Deep Dive (深入研究):
早期计算机技术没有现在这么先进，随机数通常依赖硬件的随机性，比如噪音啊等等。后来发展出了各种算法生成"伪随机数"——重复一个计算过程能得到一系列相同的"随机"序列，但对大多数应用来说足够随机。Clojure使用的是Java的随机数生成方式，也就是经典的"线性同余生成器"。如果需要真正的随机数，你得依赖其他的工具和库来获取外部的随机数据，如操作系统提供的熵源。



## See Also (另请参阅):
- [Clojure官网](https://clojure.org/)
- [Java Math类文档](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- [随机数生成算法维基](https://en.wikipedia.org/wiki/Random_number_generation)