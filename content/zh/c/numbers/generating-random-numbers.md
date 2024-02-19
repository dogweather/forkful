---
aliases:
- /zh/c/generating-random-numbers/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:20.685467-07:00
description: "\u5728 C \u8BED\u8A00\u4E2D\u751F\u6210\u968F\u673A\u6570\u6D89\u53CA\
  \u521B\u5EFA\u4E0D\u53EF\u9884\u6D4B\u7684\u503C\uFF0C\u5E76\u4E14\u8FD9\u4E9B\u503C\
  \u9075\u5FAA\u7279\u5B9A\u7684\u5206\u5E03\uFF0C\u4F8B\u5982\u5747\u5300\u5206\u5E03\
  \u6216\u6B63\u6001\u5206\u5E03\u3002\u8FD9\u9879\u80FD\u529B\u5BF9\u4E8E\u4ECE\u4EFF\
  \u771F\u548C\u6E38\u620F\u5230\u52A0\u5BC6\u64CD\u4F5C\u7684\u5E94\u7528\u7A0B\u5E8F\
  \u81F3\u5173\u91CD\u8981\uFF0C\u5176\u4E2D\u4E0D\u53EF\u9884\u6D4B\u6027\u6216\u6A21\
  \u62DF\u73B0\u5B9E\u4E16\u754C\u7684\u968F\u673A\u6027\u662F\u5FC5\u4E0D\u53EF\u5C11\
  \u7684\u3002"
lastmod: 2024-02-18 23:08:59.555179
model: gpt-4-0125-preview
summary: "\u5728 C \u8BED\u8A00\u4E2D\u751F\u6210\u968F\u673A\u6570\u6D89\u53CA\u521B\
  \u5EFA\u4E0D\u53EF\u9884\u6D4B\u7684\u503C\uFF0C\u5E76\u4E14\u8FD9\u4E9B\u503C\u9075\
  \u5FAA\u7279\u5B9A\u7684\u5206\u5E03\uFF0C\u4F8B\u5982\u5747\u5300\u5206\u5E03\u6216\
  \u6B63\u6001\u5206\u5E03\u3002\u8FD9\u9879\u80FD\u529B\u5BF9\u4E8E\u4ECE\u4EFF\u771F\
  \u548C\u6E38\u620F\u5230\u52A0\u5BC6\u64CD\u4F5C\u7684\u5E94\u7528\u7A0B\u5E8F\u81F3\
  \u5173\u91CD\u8981\uFF0C\u5176\u4E2D\u4E0D\u53EF\u9884\u6D4B\u6027\u6216\u6A21\u62DF\
  \u73B0\u5B9E\u4E16\u754C\u7684\u968F\u673A\u6027\u662F\u5FC5\u4E0D\u53EF\u5C11\u7684\
  \u3002"
title: "\u751F\u6210\u968F\u673A\u6570"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 C 语言中生成随机数涉及创建不可预测的值，并且这些值遵循特定的分布，例如均匀分布或正态分布。这项能力对于从仿真和游戏到加密操作的应用程序至关重要，其中不可预测性或模拟现实世界的随机性是必不可少的。

## 如何进行：

在 C 中，可以使用标准库 `<stdlib.h>` 的 `rand()` 函数生成随机数。默认情况下，`rand()` 产生的伪随机数范围是从 0 到 `RAND_MAX`（在 `<stdlib.h>` 中定义的常量）。为了更多地控制范围，程序员可以操控 `rand()` 的输出。

这是一个生成 0 到 99 之间随机数的简单示例：

```c
#include <stdio.h>
#include <stdlib.h> // 用于 rand() 和 srand()
#include <time.h>   // 用于 time()

int main() {
    // 种子化随机数生成器
    srand((unsigned) time(NULL));

    // 产生一个 0 到 99 之间的随机数
    int randomNumber = rand() % 100;

    printf("随机数：%d\n", randomNumber);

    return 0;
}
```

每次运行此程序的样本输出可能会有所不同：

```
随机数：42
```
要在不同范围内生成随机数，可以相应地调整模运算符（`%`）。例如，`rand() % 10` 生成从 0 到 9 的数字。

重要的是要注意，使用当前时间（`time(NULL)`）种子化伪随机数生成器（`srand()` 调用）确保了程序执行之间的随机数序列不同。如果不进行种子化（`srand()`），每次运行程序时 `rand()` 将产生相同的数字序列。

## 深入探索

`rand()` 函数及其种子化对应函数 `srand()` 几十年来一直是 C 标准库的一部分。它们基于的算法生成的数列看似随机——因此称为“伪随机”。`rand()` 中使用的底层算法通常是线性同余生成器（LCG）。

尽管 `rand()` 和 `srand()` 对许多应用程序足够用，但它们的随机性质量和潜在可预测性已知存在限制。对于需要高质量随机性的应用程序，如加密操作，应该考虑使用像 Unix 类系统中的 `/dev/random` 或 `/dev/urandom`，或者加密库提供的 API 等替代方案。

随着 C11 的引入，ISO C 标准加入了一个新的头文件 `<stdatomic.h>`，为并发操作提供了更精细的控制，但与随机性没有直接关系。对于在 C 中获取真正的随机性，开发者通常转向提供更好算法或使用硬件熵源的平台特定或外部库。

记住，尽管 `rand()` 作为一个简单且易于获取的产生伪随机数的手段，但它在现代应用中的用途受到了其输出的质量和可预测性的限制。当需要更健壮的解决方案时，尤其是对于安全意识强的应用程序，强烈推荐探索标准库之外的解决方案。
