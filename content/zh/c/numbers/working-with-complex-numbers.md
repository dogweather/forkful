---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:57.538429-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 C \u8BED\u8A00\u4E2D\uFF0C\u590D\
  \u6570\u7531\u6807\u51C6\u5E93\u652F\u6301\uFF0C\u5177\u4F53\u662F `<complex.h>`\u3002\
  \u8981\u4F7F\u7528\u5B83\u4EEC\uFF0C\u58F0\u660E\u53D8\u91CF\u65F6\u4F7F\u7528 `double\
  \ complex` \u7C7B\u578B\uFF08\u6216\u5355\u7CBE\u5EA6\u7684 `float complex`\uFF09\
  \u3002\u4EE5\u4E0B\u662F\u6267\u884C\u57FA\u672C\u64CD\u4F5C\u7684\u65B9\u6CD5\uFF1A\
  ."
lastmod: '2024-04-05T22:38:47.448417-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 C \u8BED\u8A00\u4E2D\uFF0C\u590D\u6570\
  \u7531\u6807\u51C6\u5E93\u652F\u6301\uFF0C\u5177\u4F53\u662F `<complex.h>`\u3002\
  \u8981\u4F7F\u7528\u5B83\u4EEC\uFF0C\u58F0\u660E\u53D8\u91CF\u65F6\u4F7F\u7528 `double\
  \ complex` \u7C7B\u578B\uFF08\u6216\u5355\u7CBE\u5EA6\u7684 `float complex`\uFF09\
  \u3002\u4EE5\u4E0B\u662F\u6267\u884C\u57FA\u672C\u64CD\u4F5C\u7684\u65B9\u6CD5\uFF1A\
  ."
title: "\u5904\u7406\u590D\u6570\u7684\u5DE5\u4F5C"
weight: 14
---

## 如何操作：
在 C 语言中，复数由标准库支持，具体是 `<complex.h>`。要使用它们，声明变量时使用 `double complex` 类型（或单精度的 `float complex`）。以下是执行基本操作的方法：

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // 声明一个复数 1+2i
    double complex z2 = 1.0 - 2.0*I; // 声明另一个复数 1-2i
    
    // 加法
    double complex sum = z1 + z2;
    printf("Sum: %.2f + %.2fi\n", creal(sum), cimag(sum)); // 输出：Sum: 2.00 + 0.00i

    // 乘法
    double complex product = z1 * z2;
    printf("Product: %.2f + %.2fi\n", creal(product), cimag(product)); // 输出：Product: 5.00 + 0.00i

    // 复共轭
    double complex conjugate = conj(z1);
    printf("Conjugate of z1: %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // 输出：Conjugate of z1: 1.00 - 2.00i
    
    // 幅度
    double magnitude = cabs(z1);
    printf("Magnitude of z1: %.2f\n", magnitude); // 输出：Magnitude of z1: 2.24

    // 相位
    double phase = carg(z1);
    printf("Phase of z1: %.2f\n", phase); // 输出以弧度为单位
    
    return 0;
}
```
注意，`I` 是 `<complex.h>` 中表示虚数单位的常量。函数如 `creal()` 和 `cimag()` 分别用于提取实部和虚部，而 `conj()` 计算复共轭。对于复数的幅度和相位（论证），使用 `cabs()` 和 `carg()`。

## 深入了解
C 语言中对复数的支持相对较新，已经在 C99 中标准化。在此之前，C 语言中的复数算术操作是麻烦的，通常需要自定义数据结构和函数。引入 `<complex.h>` 和复数数据类型大大增强了语言在科学和工程应用方面的能力。然而，值得注意的是，一些语言，如 Python，通过内置数据类型和更丰富的库函数集提供了对复数更直观的支持。尽管如此，C 语言在高性能计算任务中提供的性能和控制使它成为首选，即使这意味着在进行复数算术时需要处理稍微更繁琐的语法。
