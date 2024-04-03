---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:57.538429-07:00
description: "\u590D\u6570\u7531\u5B9E\u90E8\u548C\u865A\u90E8\u7EC4\u6210\uFF0C\u8868\
  \u793A\u4E3A `a + bi`\uFF0C\u5176\u4E2D `i` \u662F `-1` \u7684\u5E73\u65B9\u6839\
  \u3002\u7A0B\u5E8F\u5458\u5728\u5404\u4E2A\u9886\u57DF\uFF08\u5982\u7535\u6C14\u5DE5\
  \u7A0B\u3001\u91CF\u5B50\u8BA1\u7B97\u548C\u6D41\u4F53\u52A8\u529B\u5B66\uFF09\u4F7F\
  \u7528\u590D\u6570\uFF0C\u5229\u7528\u5B83\u4EEC\u7684\u72EC\u7279\u5C5E\u6027\u8FDB\
  \u884C\u6A21\u62DF\u3001\u4FE1\u53F7\u5904\u7406\u548C\u89E3\u51B3\u7279\u5B9A\u7C7B\
  \u578B\u7684\u6570\u5B66\u65B9\u7A0B\u3002"
lastmod: '2024-03-13T22:44:48.309666-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\u7531\u5B9E\u90E8\u548C\u865A\u90E8\u7EC4\u6210\uFF0C\u8868\
  \u793A\u4E3A `a + bi`\uFF0C\u5176\u4E2D `i` \u662F `-1` \u7684\u5E73\u65B9\u6839\
  \u3002\u7A0B\u5E8F\u5458\u5728\u5404\u4E2A\u9886\u57DF\uFF08\u5982\u7535\u6C14\u5DE5\
  \u7A0B\u3001\u91CF\u5B50\u8BA1\u7B97\u548C\u6D41\u4F53\u52A8\u529B\u5B66\uFF09\u4F7F\
  \u7528\u590D\u6570\uFF0C\u5229\u7528\u5B83\u4EEC\u7684\u72EC\u7279\u5C5E\u6027\u8FDB\
  \u884C\u6A21\u62DF\u3001\u4FE1\u53F7\u5904\u7406\u548C\u89E3\u51B3\u7279\u5B9A\u7C7B\
  \u578B\u7684\u6570\u5B66\u65B9\u7A0B\u3002."
title: "\u5904\u7406\u590D\u6570\u7684\u5DE5\u4F5C"
weight: 14
---

## 什么和为什么？

复数由实部和虚部组成，表示为 `a + bi`，其中 `i` 是 `-1` 的平方根。程序员在各个领域（如电气工程、量子计算和流体动力学）使用复数，利用它们的独特属性进行模拟、信号处理和解决特定类型的数学方程。

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
