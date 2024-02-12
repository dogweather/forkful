---
title:                "处理复数"
aliases: - /zh/cpp/working-with-complex-numbers.md
date:                  2024-01-26T04:38:01.498001-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理复数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
复数通过添加一个虚数单位“i”来扩展实数，其中 i^2 = -1。程序员们使用它们进行仿真、信号处理以及解决需要在两个维度中工作的数学问题。

## 怎么做：
C++有一个内建库`<complex>`，它简化了复数的操作。这里是一个快速查看：

```cpp
#include <iostream>
#include <complex>

int main() {
    std::complex<double> num1(2.0, 3.0); // 创建一个复数 (2 + 3i)
    std::complex<double> num2(3.0, 4.0); // 另一个复数 (3 + 4i)

    // 加法
    std::complex<double> result = num1 + num2;
    std::cout << "加法结果: " << result << std::endl; // (5 + 7i)

    // 乘法
    result = num1 * num2;
    std::cout << "乘法结果: " << result << std::endl; // (-6 + 17i)

    // 共轭
    result = std::conj(num1);
    std::cout << "num1的共轭: " << result << std::endl; // (2 - 3i)
    
    return 0;
}
```

## 深入探讨
复数有着丰富的历史，最早出现在16世纪解立方方程的解中。它们在许多领域都是必不可少的，不仅仅是在编程中。在计算机科学内部，复数帮助实现需要二维数值空间的算法，如快速傅立叶变换（FFT）。

虽然C++的`<complex>`库是标准的，但其他语言中也存在替代品，如Python的`complex`数据类型或JavaScript的数学库。`<complex>`库本身提供了全面的功能，包括为复数量身定制的三角、指数和对数运算。

在编程这些数时，理解其背后的数学原理是关键，以防止不准确并理解像复数共轭这样的操作，这种操作改变了虚部的符号，或者欧拉公式的含义，该公式将复指数与三角函数相关联。

## 参见
- C++标准模板库文档：https://en.cppreference.com/w/cpp/header/complex
- 关于复数的更深入的数学探讨：https://mathworld.wolfram.com/ComplexNumber.html
- 用于可视化，Python库Matplotlib可以绘制复数：https://matplotlib.org/
