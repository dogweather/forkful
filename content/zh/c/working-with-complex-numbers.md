---
title:                "处理复数"
date:                  2024-01-26T04:37:45.048903-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理复数"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
复数，即包含实部与虚部（如 3 + 4i）的数，是高级计算中的关键，例如信号处理或解决特定方程。程序员在传统数字无法满足需求的数学密集型应用程序中处理它们。

## 如何使用：
自 C99 以来，C 语言就原生支持复数类型和库。以下是如何使用它的方法：

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // 声明两个复数
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // 复数的操作
    double complex sum = z1 + z2;
    double complex mult = z1 * z2;

    // 打印结果
    printf("和: %.1f + %.1fi\n", creal(sum), cimag(sum));
    printf("积: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // 绝对值和相位角
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

示例输出：
```
和: 3.0 + 1.0i
积: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```
## 深入探讨
复数的历史可以追溯到几百年前，起源于16世纪的代数。快进到现在，它们在许多编程语言中都是基础，不仅仅是在 C。

C99 标准引入了 `<complex.h>`，这是一个头文件，定义了宏、函数以及 `complex` 数据类型。虽然存在替代方案 - 如创建自己的结构，但何必重新发明轮子？C 标准库已经经过优化，随时可用。

尽管 C 的复数支持颇具实力，但它并非没有批评者。与 Python 等语言的类似功能相比，它可能不那么直观，处理特殊情况也可能变得棘手。但是，就原始性能而言，它仍然是一个坚实的选择。

## 参见
- C99 标准中关于 `<complex.h>` 的文档：https://en.cppreference.com/w/c/numeric/complex
- 浮点运算的 IEEE 标准（IEEE 754）：https://ieeexplore.ieee.org/document/4610935
- C 语言复数数学在线教程：https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming
