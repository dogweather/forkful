---
title:                "Haskell: 生成随机数"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么要生成随机数？

生成随机数在编程中是一项非常重要的技能。它可以让我们在游戏、密码学和统计学等领域中模拟随机事件，也可以使我们的数据集更加多样化和真实。在Haskell中，我们可以轻松地生成高质量的随机数，让我们一起来看看如何实现吧！

## 如何实现

在Haskell中，我们可以使用标准函数库中的 `random` 模块来生成随机数。让我们来看一个简单的例子，在控制台中打印出一个1~10之间的随机数：

```Haskell
import System.Random

main = do
    gen <- getStdGen
    let (num, _) = randomR (1,10) gen
    print num
```

我们首先引入 `System.Random` 模块，然后使用 `getStdGen` 函数来生成一个随机数生成器 `gen`。接下来，我们使用 `randomR` 函数来指定随机数的范围，并将结果存储在 `num` 变量中。最后，我们打印出这个随机数。运行代码，你会看到每次运行都会得到一个不同的随机数。

我们也可以生成一个特定类型的随机数，比如生成一个随机的布尔值：

```Haskell
import System.Random

main = do
    gen <- getStdGen
    let (bool, _) = random gen :: (Bool, StdGen)
    print bool
```

这里我们使用了类型注释 `::` 来指定需要生成的随机数类型，然后打印出 `bool` 值。

## 深入探讨

在Haskell中，生成随机数的主要过程是通过伪随机数生成器来实现的。伪随机数生成器使用一个种子值来生成序列中的每一个随机数，因此同一个种子值生成的随机数序列是一样的。为了获得真正随机的的结果，我们可以使用 `newStdGen` 函数来每次生成一个新的种子值。

除了 `randomR` 和 `random` 函数，我们还可以使用 `randoms` 函数来生成一个无限的随机数列表，以及 `randomRs` 函数来生成一个无限的随机数范围列表。这些函数可以帮助我们在更复杂的随机数需要中很有用。

## 参考链接

- [Haskell随机数文档](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [了解有关随机数生成器的更多信息](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)

# 参考资料

## 见下文

- [Haskell Wiki - 随机数](https://wiki.haskell.org/Random_number_generation)