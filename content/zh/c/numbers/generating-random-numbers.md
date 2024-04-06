---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:20.685467-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A \u5728 C \u4E2D\uFF0C\u53EF\u4EE5\u4F7F\
  \u7528\u6807\u51C6\u5E93 `<stdlib.h>` \u7684 `rand()` \u51FD\u6570\u751F\u6210\u968F\
  \u673A\u6570\u3002\u9ED8\u8BA4\u60C5\u51B5\u4E0B\uFF0C`rand()` \u4EA7\u751F\u7684\
  \u4F2A\u968F\u673A\u6570\u8303\u56F4\u662F\u4ECE 0 \u5230 `RAND_MAX`\uFF08\u5728\
  \ `<stdlib.h>` \u4E2D\u5B9A\u4E49\u7684\u5E38\u91CF\uFF09\u3002\u4E3A\u4E86\u66F4\
  \u591A\u5730\u63A7\u5236\u8303\u56F4\uFF0C\u7A0B\u5E8F\u5458\u53EF\u4EE5\u64CD\u63A7\
  \ `rand()` \u7684\u8F93\u51FA\u3002\u2026"
lastmod: '2024-04-05T22:38:47.450885-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u8FDB\u884C\uFF1A \u5728 C \u4E2D\uFF0C\u53EF\u4EE5\u4F7F\u7528\
  \u6807\u51C6\u5E93 `<stdlib.h>` \u7684 `rand()` \u51FD\u6570\u751F\u6210\u968F\u673A\
  \u6570\u3002\u9ED8\u8BA4\u60C5\u51B5\u4E0B\uFF0C`rand()` \u4EA7\u751F\u7684\u4F2A\
  \u968F\u673A\u6570\u8303\u56F4\u662F\u4ECE 0 \u5230 `RAND_MAX`\uFF08\u5728 `<stdlib.h>`\
  \ \u4E2D\u5B9A\u4E49\u7684\u5E38\u91CF\uFF09\u3002\u4E3A\u4E86\u66F4\u591A\u5730\
  \u63A7\u5236\u8303\u56F4\uFF0C\u7A0B\u5E8F\u5458\u53EF\u4EE5\u64CD\u63A7 `rand()`\
  \ \u7684\u8F93\u51FA\u3002 \u8FD9\u662F\u4E00\u4E2A\u751F\u6210 0 \u5230 99 \u4E4B\
  \u95F4\u968F\u673A\u6570\u7684\u7B80\u5355\u793A\u4F8B\uFF1A."
title: "\u751F\u6210\u968F\u673A\u6570"
weight: 12
---

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
