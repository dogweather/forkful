---
title:                "生成随机数"
date:                  2024-01-20T17:49:04.693620-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么要用？)
生成随机数就是创建不可预测的数字。程序员用它们做测试、模拟和安全性任务。

## How to: (如何实现：)
Haskell 用 `random` 库生成随机数。以下是一个简单示例：

```Haskell
import System.Random

main :: IO ()
main = do
    gen <- getStdGen
    print $ take 5 (randoms gen :: [Int])
```

预期输出是五个随机整数，每次运行都不一样。

## Deep Dive (深入研究)
早期，随机数生成依赖基础算法，如线性同余发生器。现在有更复杂的算法如梅森旋转法。`random` 是 Haskell 标准，但还有其他库，如 `mwc-random` 和 `pcg-random` 提供性能改进和不同特征。真正的随机数（不是伪随机）需要物理过程，软件仅能接近。

## See Also (延伸阅读)
- Haskell `random` 库文档：https://hackage.haskell.org/package/random
- 关于随机数生成的维基页面：https://en.wikipedia.org/wiki/Random_number_generation
- [`mwc-random` 库](https://hackage.haskell.org/package/mwc-random)
- [`pcg-random` Haskell 包](https://hackage.haskell.org/package/pcg-random)