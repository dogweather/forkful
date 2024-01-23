---
title:                "生成随机数"
date:                  2024-01-20T17:48:38.179086-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么?)

生成随机数是让计算机产出无法预测的数值的过程。程序员需要随机数来模拟随机事件、测试算法的健壮性，或者在游戏中创建不可预测性。

## How to: (如何操作：)

在 C++ 中，你可以使用 `<random>` 头文件中的工具来生成随机数。看下面的例子：

```C++
#include <iostream>
#include <random>

int main() {
    std::random_device rd;  // 生成随机数的设备
    std::mt19937 gen(rd()); // 利用题设种子初始化 Mersenne Twister 引擎
    std::uniform_int_distribution<> distrib(1, 6); // 定义分布范围

    for (int n=0; n<10; ++n)
        std::cout << distrib(gen) << ' '; // 生成并打印随机数
    std::cout << std::endl;

    return 0;
}
```

样本输出可能是:
```
4 2 3 5 6 3 1 4 2 3
```
注意：每次运行你得到的结果可能完全不同。

## Deep Dive (深入了解)

随机数生成有历史悠久，从简单的线性同余生成器到复杂的 Mersenne Twister。`<random>` 头文件提供了多种工具，可以产生高质量的随机数。`std::random_device` 是获取非确定性随机数的硬件源。`std::mt19937` 是一个基于 Mersenne Twister 算法的伪随机数生成器，它比较适合复杂应用，如模拟或游戏。`std::uniform_int_distribution` 用于从指定的数值范围内均匀选取随机数。

替代方案包括直接使用 `rand()` 和 `srand()` 函数，但它们提供的随机数质量较低，且不能很好地支持大范围和高精度需求。选择正确的随机数生成器和分布模型对于确保结果的随机性至关重要。

## See Also (另请参阅)

- C++标准库文档 [<random> - cppreference.com](https://en.cppreference.com/w/cpp/header/random)。
- 关于伪随机数生成器： [Mersenne Twister - Wikipedia](https://en.wikipedia.org/wiki/Mersenne_Twister)。
- 给初学者的随机数引导：[Random number generation - learncpp.com](https://www.learncpp.com/cpp-tutorial/random-number-generation/)。
