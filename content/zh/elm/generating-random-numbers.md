---
title:                "生成随机数"
html_title:           "Elm: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么是生成随机数？为什么程序员需要这么做？

生成随机数是在计算机编程中常见的一种操作，它可以让我们产生一个随机的数字或者序列。程序员通常会在创建游戏、模拟实验或者加密数据时需要生成随机数。

## 如何实现：

```Elm
import Random exposing (..)

randomInt : Int
randomInt =
    generate int (range 1 10)
```
生成的结果可能是 5、8、2 等等。我们可以通过调整 ```range``` 函数内的参数来指定随机数的范围。

## 深入探讨：

在编程历史上，生成随机数一直是一个有挑战性的问题。因为计算机程序其实是按照一定的算法运行的，所以它们本质上是不能产生真正的随机数。程序员通常通过伪随机数生成器来模拟随机数，这类似于从一个看似无序的序列中挑选出一个数作为随机数。

除了 Elm 中的 ```Random``` 模块，还有其他语言也提供了生成随机数的方法，比如 JavaScript 中的 ```Math.random()``` 函数。

## 参考资料：

[Elm 文档中的 Random 模块介绍](https://package.elm-lang.org/packages/elm/random/latest/)

[CSDN 文章：随机数算法](https://blog.csdn.net/u013368721/article/details/77977066)

[Wikipedia：伪随机数生成器](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)