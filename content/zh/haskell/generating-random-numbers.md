---
title:    "Haskell: 生成随机数字"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

为什么：为什么会想要生成随机数？一句或两句话解释*为什么*有人会对生成随机数感兴趣。

大多数编程语言都内置了生成随机数的功能，Haskell也不例外。生成随机数可以用于模拟实验、密码学和游戏开发等领域。无论你是想让程序变得更加有趣，还是想要测试某种假设，生成随机数都是一个有用的工具。

## 如何使用

在Haskell中，我们可以使用`random`模块来生成随机数。首先，我们需要导入该模块：

```Haskell
import System.Random
```

接下来，我们可以使用`randomIO`函数来生成一个随机整数：

```Haskell
randomIO :: IO Int
```

这个函数返回一个被包装在`IO`类型中的整数，因为生成随机数是一个需要IO操作的过程。所以我们可以用`<-`运算符来获取实际的整数值：

```Haskell
randInt <- randomIO
```

我们也可以使用`randomRIO`函数来生成指定范围内的随机数。比如我们想要在1到10之间生成一个随机整数，可以这样写：

```Haskell
randInt <- randomRIO (1, 10)
```

## 深入探讨

生成随机数的过程其实是根据一个起始的种子值，通过一系列算法来计算出下一个随机数。Haskell中使用的是Mersenne Twister算法来生成随机数，这是一种伪随机数生成算法，也就是说它实际上并不是真正的随机数，但是在实践中可以满足我们的需求。

每次重新启动程序后，`randomIO`都会生成相同的随机数，因为它使用的是程序启动时的默认种子。而`randomRIO`的种子则可以手动指定，这样每次生成的随机数就不会重复。比如我们可以使用当前时间作为种子：

```Haskell
import Data.Time.Clock

currentTime <- getCurrentTime
gen <- newStdGen currentTime
randInt <- randomRIO (1,10) gen
```

这样每次程序运行时，`currentTime`会作为一个新的种子，确保每次生成的随机数都不同。

## 参考链接

- [Haskell的random模块文档](https://hackage.haskell.org/package/random)
- [Mersenne Twister算法介绍](https://en.wikipedia.org/wiki/Mersenne_Twister)

## 参考链接

- [Haskell的随机模块文档](https://hackage.haskell.org/package/random)
- [Mersenne Twister算法介绍](https://en.wikipedia.org/wiki/Mersenne_Twister)