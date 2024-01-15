---
title:                "生成随机数"
html_title:           "C: 生成随机数"
simple_title:         "生成随机数"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

生成随机数在编程中是非常常见的需求。它可以用来模拟现实情况、测试算法的可靠性、增加程序的复杂度以及提高程序的安全性。因此，掌握如何生成随机数是非常重要的。

## 如何生成随机数

要在C语言中生成随机数，我们需要用到`rand()`函数，并结合`time()`函数来获取一个种子。让我们来看一个简单的例子：

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    int random_number;
    // 使用时间作为种子，确保每次运行都会生成不同的随机数
    srand(time(NULL));
    // 生成随机数，范围在0到99之间
    random_number = rand() % 100;
    // 打印结果
    printf("随机数为：%d", random_number);
    return 0;
}
```

输出结果可能为：`随机数为：27`

## 进一步了解

`rand()`函数在每次调用时都会返回一个伪随机数，它是根据上一次调用所生成的随机数来计算的。因此，我们需要使用`time()`函数来获取一个不同的种子，以确保每次生成的随机数都不同。此外，`rand()`函数的取值范围通常是0到`RAND_MAX`之间，其中`RAND_MAX`是一个宏，它通常在stdlib.h中定义。

如果需要生成更大范围的随机数，我们可以使用`div()`函数来将`rand()`的返回值按照需求进行缩放。同时，对于需要高质量的随机数，我们可以使用更复杂的随机数生成算法，如线性同余算法和Mersenne Twister算法。

## 参考链接

- [`rand()`函数文档](https://en.cppreference.com/w/cpp/numeric/random/rand)
- [`time()`函数文档](https://en.cppreference.com/w/cpp/chrono/c/time)
- [线性同余算法详解](https://en.wikipedia.org/wiki/Linear_congruential_generator)
- [Mersenne Twister算法详解](https://en.wikipedia.org/wiki/Mersenne_Twister)

## 参考资料

- C语言编程指南，第四版，凯文·奈尔森，1994年，Peachpit Press出版。
- 在C中生成随机数，C语言游戏设计指南，Michael Morrison，2004年，New Riders出版社。