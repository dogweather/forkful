---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么&为什么?

生成随机数是一种在编程中产生不可预测输出的方式，经常被用于游戏、数据分析和系统安全性工作。

## 如何实现:

我们使用 C 语言的 `<stdlib.h>` 库来生成随机数。下面是一个简单示例，它使用 `rand()` 函数来生成一个随机整数。

```C
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(0));  // 使用当前时间作为种子
    int random_number = rand();  // 生成随机整数
    printf("%d\n", random_number);  // 打印随机整数
    return 0;
}
```
你的输出可能看起来像这样：

```C
1637213138
```

## 深入研究 :

生成随机数的历史可以追溯到计算机的早期时代，当时的主要目标是为模拟和统计分析应用生成伪随机数。这种需求随着时间的推移有所变化，但基本的方法却保持了下来。我们还可以使用其他方式生成随机数，例如 `/dev/random` 或 `/dev/urandom` 在 Unix-like 系统上。

注意，`rand()` 函数实际上只生成了伪随机数，这是因为它基于某个种子产生的数字序列是固定的。这也就是为什么我们在上述示例中使用 `srand(time(0))` 来为每次运行提供不同的种子。

## 看看这些：

如果你想了解更多关于生成随机数的知识，可以查看以下资源：

1. 理解 C语言中的随机数生成：https://stackoverflow.com/questions/822323/how-to-generate-a-random-number-in-c
2. 随机数生成的历史回顾：https://en.wikipedia.org/wiki/History_of_randomness
3. 介绍伪随机数的精准理论：https://en.wikipedia.org/wiki/Pseudorandom_number_generator
4. 全面探索随机数生成：https://www.random.org/randomness/