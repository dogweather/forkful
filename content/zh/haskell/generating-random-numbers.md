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

## 什么是随机数？为什么程序员要用它？

随机数是在编程中使用的一种特殊类型的数字，它在每次运行程序时都会生成不同的值。程序员使用随机数来生成随机的数据或者模拟真实世界的不确定性。例如，在游戏开发中，随机数可以用来生成随机的游戏关卡布局或者敌人的位置。

## 如何使用

Haskell中有一个内置的模块`System.Random`，它包含了生成随机数的函数。下面是一个随机产生20到50之间整数的例子：

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  num <- randomRIO (20, 50)
  print num
```

输出示例：
```
32
```

要产生不同类型的随机数，可以根据需要调整`randomRIO`函数中的参数范围，例如`(20.0, 50.0)`可以生成20到50之间的浮点数。

## 深入了解

在计算机科学中，随机数生成算法有着悠久的历史。最早的随机数算法是基于物理现象（如掷骰子、抽卡等）产生的，随后发展出利用数学公式生成的伪随机数算法。在Haskell中使用的随机数算法是基于Mersenne Twister算法，它具有很高的性能和均匀的分布特性。

除了Haskell内置的`System.Random`模块，也可以使用第三方库`random`来生成随机数。此外，还可以借助外部硬件设备（如热噪声发生器）来产生真随机数。

## 参考资料

- [Haskell中的随机数生成](https://wiki.haskell.org/Random_number_generation)
- [Mersenne Twister算法](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [random库文档](https://hackage.haskell.org/package/random)