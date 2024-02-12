---
title:                "生成随机数"
date:                  2024-01-27T20:33:28.494321-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在编程中生成随机数涉及创建一系列不具有任何可预测顺序或模式的数字。程序员经常利用这些数字用于各种目的，如模拟不可预测的事件，在测试和调试中，以及在游戏算法中确保公平性或不可预测性。

## 如何操作：

要在C++中生成随机数，你通常会使用 `<random>` 头文件，它在C++11中被引入，提供了广泛的设施用于从各种分布中生成随机数。

```C++
#include <iostream>
#include <random>

int main() {
    // 初始化一个随机数引擎
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // 定义范围[0, 99]（包含）
    std::uniform_int_distribution<> distrib(0, 99); 

    // 在定义的范围内生成并打印5个随机数
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

此代码示例使用来自 `std::random_device` 的种子初始化了一个梅森旋转随机数生成器。然后它定义了一个在[0, 99]范围内的均匀整数分布，并最终从这个分布中输出5个随机数。

示例输出可能如下所示，但请记住每次执行可能会产生不同的结果：

```
45 67 32 23 88
```

## 深入探讨：

在历史上，C++中的随机数生成严重依赖于 `<cstdlib>` 头文件中的 `rand()` 函数和用于种子生成的 `srand()` 函数。然而，这种方法经常因其在生成的数的分布上缺乏均匀性和可预测性而受到批评。

`<random>` 头文件在C++11中的引入标志了一个重大改进，提供了一个复杂的系统用于产生随机数。所提供的设施包括各种引擎（如对于梅森旋转的 `std::mt19937`）和分布（如对整数的均匀分布的 `std::uniform_int_distribution`），这些可以组合以满足程序员的特定需要，导致更可预测的行为，更好的性能和更大的灵活性。

虽然 `<random>` 库比旧的 `rand()` 方法好得多，但值得注意的是，生成真正的随机数——尤其是对于加密目的——仍然依赖于额外的考虑。对于加密应用，应该使用为安全性设计的库，这些库通常利用硬件熵源。