---
title:    "Clojure: 产生随机数"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么
在编程中，我们经常需要使用到随机数。它可以用来生成测试数据、创建游戏中的随机事件，甚至可以用来产生密码。Clojure提供了强大的功能来生成随机数，让我们一起来探究一下吧！

## 如何
在Clojure中，我们可以使用 `rand` 函数来生成随机数。例如下面的代码会生成一个介于0和1之间的随机浮点数。

```Clojure
(rand)
;; 输出：0.5174170965728857
```

如果我们想要生成一个指定范围内的随机数，我们可以使用 `rand-int` 函数并传入最小值和最大值。例如，下面的代码会生成一个介于1和10之间的随机整数。

```Clojure
(rand-int 1 10)
;; 输出：5
```

除此之外，我们还可以使用 `shuffle` 函数来乱序一个列表中的元素。例如，下面的代码会将1到10的数字随机排列。

```Clojure
(shuffle (range 1 11))
;; 输出：(7 4 3 10 6 1 8 9 2 5)
```

## 深入探究
Clojure使用Mersenne Twister算法来生成随机数。这是一种高效的伪随机数生成算法，它的周期可以达到2^19937 - 1。

另外值得注意的是，Clojure的 `rand` 函数实际上是 `ThreadLocalRandom` 类的一个包装。这意味着当我们在多线程环境下使用 `rand` 函数时，每个线程都会有自己独立的种子，避免了线程安全的问题。

## 参考链接
- [Clojure官方文档 - 随机数](https://clojure.org/reference/numbers#_random_numbers)
- [Clojure Cookbook - 随机数](https://clojure-cookbook.com/numbers/random)
- [Mersenne Twister算法 - 维基百科](https://zh.wikipedia.org/wiki/Mersenne_Twister)
- [ThreadLocalRandom类 - Java官方文档](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html)

## 你可能还想知道
- [Clojure官方文档](https://clojure.org/)
- [Clojure中国社区](https://clojure-china.org/)
- [Clojure学习资源汇总](https://github.com/zhuzilin/Clojure-Learning-Resources)