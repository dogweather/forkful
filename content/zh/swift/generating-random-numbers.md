---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么与为什么？

随机数的生成是指在预定义的范围内获取不规律的数。这对于开发游戏、数据加密，甚至是测试代码来说都是必须的。

## 如何做？

在Swift里生成随机数很直接。一般的，我们都会用`Int.random`方法来获取一个范围内的随机整数。

```Swift
let randomInt = Int.random(in: 1..<10)
print(randomInt)
```
输出可能是：
```
3
```
如果我们要一个随机的布尔值，就会用`Bool.random()`。

```Swift
let randomBool = Bool.random()
print(randomBool)
```
输出可能是：
```
false
```

## 深入探讨

历史上，我们生成随机数的方法有很多种。例如，"中间平方法"就是最早的一种方法。而现在，我们所使用的大部分算法都会基于硬件，因此速度更快，生成的数字更加随机。

除了Swift的内建方法，我们还可以使用像GameplayKit中的随机数生成器。而对于复杂项目，我们还可以使用更强大的库，比如NumPy。

在Swift中，随机数生成器的实现基于Cryptographic Module Level-1，在没有外部输入的情况下能够生成足够的随机数。

## 查看更多

1. [Apple的Swift文档](https://developer.apple.com/documentation/swift/)
2. [GameplayKit指南](https://developer.apple.com/documentation/gameplaykit/)
3. [NumPy用户指南](https://numpy.org/doc/stable/index.html)