---
title:                "生成随机数"
aliases:
- zh/haskell/generating-random-numbers.md
date:                  2024-01-27T20:34:15.268069-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？

在 Haskell 中生成随机数意味着创建出按人类标准无法预测的数值。这在从密码学应用到需要准确模拟真实世界现象的模拟中，包含机遇元素的场景中至关重要。

## 如何操作：

要在 Haskell 中生成随机数，通常使用的是 Haskell 平台的一部分——`random`包。以下是逐步指南：

首先，确保你安装了`random`包。如果没有，可以通过 Cabal 或 Stack 获取。

### 生成一个随机数

要生成一个简单的随机数，你可以使用`randomRIO`函数，它会在指定的范围内产生一个随机值。

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 10) :: IO Int
  putStrLn $ "随机数: " ++ show randomNumber
```

### 生成一个随机数列表

生成一个随机数列表稍微复杂一些，但仍然很直接：

```Haskell
import System.Random (randomRIO)

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
  r <- randomRIO (1, 100)
  rs <- randomList (n-1)
  return (r:rs)

main :: IO ()
main = do
  numbers <- randomList 5
  print numbers
```

这段代码片段创建了一个函数`randomList`，用于生成一个随机整数列表。将`(1, 100)`替换为你想要的范围。

## 深入了解

Haskell的`random`包提供了一个伪随机数生成器（PRNG），这意味着生成的数并非真正的随机，但对于许多应用来说，它们看起来足够随机了。Haskell随机生成能力的核心在于`RandomGen`类型类，该类型类抽象了生成随机数的不同方法，以及`Random`类型类，其中包括可以随机生成的类型。

从历史上看，Haskell对随机数生成的方法强调纯度和可复现性。这就是为什么涉及随机性的操作明确在`IO`单子中处理，或者需要手动传递和更新生成器状态——为了保持引用透明性。

在某些应用中，如加密学，默认PRNG生成的伪随机数可能不够安全。对于这些用例，Haskell程序员常常转向更专门的库，如`crypto-random`，这些库旨在满足加密应用的严格要求。

此外，像`mwc-random`这样的替代库通过实现现代算法如梅森旋转器，为模拟和其他应用提供了更好的性能和随机数质量。

在 Haskell 中选择随机数生成方法时，至关重要的是根据应用对随机性质量、性能和安全性的需求，来选择最合适的工具或库。
