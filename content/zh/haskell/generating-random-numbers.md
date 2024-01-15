---
title:                "生成随机数"
html_title:           "Haskell: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

##为什么

生成随机数在编程中是一个常见的需要，它可以用来模拟不确定性或者给程序增加一些随机性。这在开发游戏、进行数据分析和测试算法等方面都有很大的用处。

##如何进行

首先，我们需要导入一个名为Random的Haskell模块。这个模块提供了生成随机数的函数。

```Haskell
import System.Random
```

接下来，我们可以使用`random`函数生成一个随机整数。第一个参数是一个范围，第二个参数是一个随机数生成器。

```Haskell
random (1, 10) (mkStdGen 42) -- 输出随机数为7
```

我们也可以使用`randomR`函数生成指定范围内的随机数。

```Haskell
randomR (0.0, 1.0) (mkStdGen 42) -- 输出随机数为0.639182078298092
```

如果我们需要生成多个随机数，可以使用`randoms`函数。它会返回一个无限列表，所以我们需要限制它的数量。

```Haskell
take 5 (randoms (mkStdGen 42)) -- 输出[642,7781000,-263351296,1244105165,-765208830]
```

##深入探讨

在Haskell中，随机数生成器的种子是一个重要的概念。种子决定了随机数的序列，所以使用相同的种子生成的随机数序列也是相同的。

我们可以使用`getStdGen`函数获取一个系统提供的随机数生成器。但是，这个随机数生成器每次生成的随机数都是相同的。为了让随机数更随机，我们需要使用不同的种子，比如当前系统时间。

```Haskell
getStdGen -- 生成的随机数序列将在每次运行程序时都相同
```

```Haskell
newStdGen -- 生成的随机数序列将使用当前系统时间作为种子，所以每次运行都不同
```

##参考链接

- [Haskell Random Module](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
- [Haskell School of Expression: Randomness](https://www.cs.bham.ac.uk/~drg/learning/randomness.pdf)
- [Haskell Random Numbers: Pseudo or Real?](https://stackoverflow.com/questions/3078093/haskell-random-numbers-pseudo-or-real)

##参见

- [Haskell官方文档](https://www.haskell.org/documentation)
- [Haskell实用指南](https://github.com/bitemyapp/learnhaskell)