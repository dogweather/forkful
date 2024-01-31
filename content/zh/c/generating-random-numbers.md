---
title:                "生成随机数"
date:                  2024-01-27T20:33:42.201726-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？

在C语言中生成随机数涉及到创建一系列没有任何可辨识模式的数字，模仿随机性的概念。程序员利用随机数为众多目的提供支持，包括模拟数据、加密应用和游戏开发，这使得它成为编程的一个重要方面。

## 如何操作：

要在C语言中生成随机数，你通常会使用 `stdlib.h` 中的 `rand()` 函数。然而，为了确保在不同程序执行中生成的数字变化，非常关键的一步是对随机数生成器进行种子设定。使用 `srand()` 函数，并以一个值（通常是当前时间）作为种子，就可以实现这一点。

这里有一个生成0到99之间随机数的简单示例：

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // 对随机数生成器进行种子设定
    srand((unsigned) time(NULL));

    // 生成一个0到99之间的随机数
    int randomNumber = rand() % 100;

    // 打印该随机数
    printf("Random Number: %d\n", randomNumber);

    return 0;
}
```

样本输出：

```
Random Number: 42
```

需要注意的是，由于每次执行此程序时都使用当前时间进行种子设定，因此每次都会产生一个新的随机数。

## 深入探索

C语言中传统的生成随机数方式，使用 `rand()` 和 `srand()`，其实并不是真正的随机。这是伪随机的。对于许多应用来说这种方法是可以的，但在需要高度随机性的情况下，比如在严肃的加密应用中，就表现得不够充分。`rand()` 生成的序列完全由提供给 `srand()` 的种子确定。因此，如果种子是已知的，那么序列就可以被预测，降低了随机性。

从历史上看，`rand()` 函数因其低质量的随机性和有限的范围而受到批评。现代替代方法包括使用设备特定的API或外部库，这些方法能更好地近似真正的随机性，或者，在UNIX-like系统中，为加密目的从 `/dev/random` 或 `/dev/urandom` 读取。

例如，在C语言中使用 `/dev/urandom`：

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // 为了读取打开 /dev/urandom
    fp = fopen("/dev/urandom", "r");

    // 读取一个随机数
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // 打印该随机数
    printf("Random Number: %u\n", randomNumber);

    // 关闭文件
    fclose(fp);

    return 0;
}
```

这种方法直接从系统的熵池中读取，提供了更高质量的随机性，适用于更敏感的应用。然而，这种方式在不同平台上可能会有可移植性问题，使其不如使用 `rand()` 通用。

无论采用哪种方法，理解随机性的本质及其在C语言中的实现，对于开发有效、安全和吸引人的应用至关重要。
