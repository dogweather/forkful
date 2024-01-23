---
title:                "生成随机数"
date:                  2024-01-20T17:49:16.758189-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么？)

在编程中，生成随机数字用于创造不可预测性，比如游戏中的随机事件或数据分析的采样。程序员这么做是为了模拟现实世界的随机性或者测试各种可能出现的场景。

## How to: (如何做：)

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // 初始化随机数发生器，使每次程序运行产生不同的随机数
    srand(time(0));

    // 生成一个随机数在 [0, 99]
    int random_number = rand() % 100;
    printf("Random Number: %d\n", random_number);

    return 0;
}
```
示例输出：
```
Random Number: 42
```
## Deep Dive (深入了解)

在历史上，随机数生成起初不是计算机领域的重点，但很快随着对模拟现实世界事件和应用需求的增长，这变得日益重要。C中的`rand()`函数，自1970年代就存在。一个主要的挑战是，计算机本质上是确定性的设备，因此产生真正的随机性很难。通常，所谓的随机数实际上是使用数学算法生成的伪随机数序列。

由于`rand()`依赖于种子，通过`srand()`函数设定种子值可以产生不同的随机数序列。如果不调用`srand()`，或总是以相同的种子调用，每次运行程序将获得相同的随机数序列。

在某些场合，`rand()`的质量可能不足。在这种情况下，标准库提供了替代方法，例如`<random>`中的功能更全面的随机数引擎和分布。

需要注意的实现细节包括随机数生成器的周期长度、产生随机数的速度以及生成的随机序列的统计特性。

## See Also (另请参阅)

- C标准库说明: https://en.cppreference.com/w/c/numeric/random
- Modern C随机数生成介绍: https://en.cppreference.com/w/c/numeric/random
- 如何在C中正确生成随机数: https://stackoverflow.com/questions/822323/how-to-generate-a-random-int-in-c
