---
title:    "Haskell: 生成随机数"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

为什么：生成随机数是程序设计中常见的需求之一。它可以用于模拟实验、加密算法和游戏等多种应用。在Haskell中，有多种方法可以生成随机数，让我们来看看如何实现它。

如何实现：Haskell提供了一个名为“random”的标准库来处理随机数。要使用它，首先我们需要导入它的模块，```:hs
import System.Random
```

使用“random”库的最基本方法是使用函数“randomR”，该函数可以生成指定范围内的随机数。例如，我们可以生成一个介于0到10之间的随机整数，示例代码如下：```:hs
randomNumber <- randomRIO (0, 10)
print randomNumber
-- 输出结果可能是：5
```

我们也可以使用“randomR”函数来生成随机的浮点数，在范围内生成随机数的代码如下：```:hs
randomNumber <- randomRIO (0.0, 10.0)
print randomNumber
-- 输出结果可能是：7.3421
```

除了“randomR”函数外，我们也可以使用“randomIO”函数来生成随机数。它会根据给定的类型，随机生成相应的值。示例如下：```:hs
randomNum :: IO Int
randomNum = randomIO
randomString :: IO String
randomString = randomIO
```

深入介绍：生成随机数的过程其实是依赖于一个随机数生成器。在Haskell中，我们可以自己定义一个随机数生成器，也可以使用系统提供的默认随机数生成器。使用系统默认的随机数生成器的代码如下：```:hs
gen <- getStdGen
randomRs (-10, 10) gen :: [Int]
```

除了“randomR”和“randomIO”这两个常用的函数外，还有其他一些有用的函数可以用来生成随机数，例如“randomRs”、“randomRsIO”和“randomShuffle”等，有兴趣的读者可以自行探索。

阅读详细文档：如果你想进一步了解Haskell中生成随机数的相关知识，可以阅读官方文档[Random](https://hackage.haskell.org/package/random)和[Tutorial](https://wiki.haskell.org/Random)来了解更多信息。

同样，你也可以在[Real World Haskell](http://book.realworldhaskell.org/read/random-data-and-changes.html)和[Haskell tutorial](https://www.haskell.org/haskellwiki/Introduction)中找到关于随机数生成的更多内容。

另外，如果你想了解如何在Haskell中使用随机数来实现游戏，可以参考[这个教程](http://yannesposito.com/Scratch/en/blog/Haskell-video-game-chapter-1/)。

另请参阅：还有其他一些库可以用来生成随机数，例如[mwc-random](https://hackage.haskell.org/package/mwc-random)和[random-fu](https://hackage.haskell.org/package/random-fu)，如果你对这方面的内容感兴趣，可以尝试使用它们。