---
title:                "Haskell: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么要使用随机数字

随机数字在编程中扮演着重要的角色，它们可以帮助我们模拟真实世界的情况，例如抽奖活动、游戏中的随机事件等。使用随机数字能够给我们的程序带来更多的变化和趣味性。

## 如何生成随机数字

要在Haskell中生成随机数字，我们可以使用`random`包提供的函数。首先，我们需要导入`System.Random`模块：

```Haskell
import System.Random
```

接着，我们可以使用`randomIO`函数来生成一个随机的`Int`类型数字：

```Haskell
randomNum <- randomIO :: IO Int
```

如果我们需要生成一个指定范围的随机数字，我们可以使用`randomRIO`函数。以下是一个生成1到100之间随机数字的例子：

```Haskell
randomNum <- randomRIO (1,100) :: IO Int
```

随机数字也可以用于生成随机的布尔值，通过使用`random`函数和`randomR`函数可以实现：

```Haskell
randomBool <- randomIO :: IO Bool
randomIntBool <- randomRIO (0,1) :: IO Int
```

## 深入了解随机数字

在计算机中生成真正的随机数字是不可能的，因为计算机是基于算法的，无法产生完全随机的数。因此，随机数字实际上是伪随机的，它们是通过种子值和某些算法生成的。

在Haskell中，我们可以通过设置种子值来控制随机数字的生成。这可以通过`mkStdGen`函数来实现，它接受一个整数作为参数，这个整数就是种子值。如果不指定种子值，默认将使用系统时间作为种子值。

## 参考链接

- [Haskell官方文档-随机数](https://www.haskell.org/documentation/#random-numbers)
- [Haskell Wiki-随机数生成](https://wiki.haskell.org/Random_number_generation)
- [Hackage-hs-random包文档](https://hackage.haskell.org/package/random) 

## 查看更多

如果您对使用Haskell生成随机数字感兴趣，可以查看以下链接获取更多信息：

- [Haskell随机数生成-掘金博客](https://juejin.cn/post/6844903918377771021)
- [随机数生成器的种子值设置-博客园](https://www.cnblogs.com/kissdodog/p/10287354.html)
- [在Haskell中模拟抽奖游戏](https://rosettacode.org/wiki/Lottery_game#Haskell)