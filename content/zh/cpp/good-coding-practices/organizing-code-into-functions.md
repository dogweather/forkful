---
date: 2024-01-26 01:09:38.566496-07:00
description: "\u5982\u4F55\u505A: \u8BA9\u6211\u4EEC\u4EE5\u4E00\u4E2A\u5E38\u89C1\
  \u7684\u4EFB\u52A1\u4E3A\u4F8B\uFF1A\u8BA1\u7B97\u5706\u7684\u9762\u79EF\u3002\u6211\
  \u4EEC\u53EF\u4EE5\u5C06\u8FD9\u4E2A\u516C\u5F0F\u5C01\u88C5\u5728\u4E00\u4E2A\u51FD\
  \u6570\u4E2D\uFF0C\u800C\u4E0D\u662F\u6BCF\u6B21\u90FD\u5199\u76F8\u540C\u7684\u8BA1\
  \u7B97\u516C\u5F0F\u3002"
lastmod: '2024-04-05T21:53:48.409955-06:00'
model: gpt-4-1106-preview
summary: "\u8BA9\u6211\u4EEC\u4EE5\u4E00\u4E2A\u5E38\u89C1\u7684\u4EFB\u52A1\u4E3A\
  \u4F8B\uFF1A\u8BA1\u7B97\u5706\u7684\u9762\u79EF\u3002\u6211\u4EEC\u53EF\u4EE5\u5C06\
  \u8FD9\u4E2A\u516C\u5F0F\u5C01\u88C5\u5728\u4E00\u4E2A\u51FD\u6570\u4E2D\uFF0C\u800C\
  \u4E0D\u662F\u6BCF\u6B21\u90FD\u5199\u76F8\u540C\u7684\u8BA1\u7B97\u516C\u5F0F\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 如何做:
让我们以一个常见的任务为例：计算圆的面积。我们可以将这个公式封装在一个函数中，而不是每次都写相同的计算公式。

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "半径为 " << r << " 的圆的面积是 " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

示例输出：
```
半径为 5 的圆的面积是 78.5397
```

## 深入探讨
从历史上看，程序和函数是结构化编程的支柱，在20世纪60年代被提出来对抗早期命令式编程语言中出现的“意大利面条代码”问题。像面向对象编程（OOP）这样的替代方案通过将这些函数与数据结构关联起来进一步发展。在 C++中，你拥有常规函数、类方法（包括静态方法）、lambdas和模板函数，每种都提供不同的好处。实现组织良好的函数通常涉及遵循像DRY（“不要重复你自己”）和SRP（单一职责原则）这样的原则，也就是说每个函数只做一件事，并且做好。

## 另请参阅
更多关于C++中的函数：
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

关于与函数相关的设计原则：
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

学习关于lambdas和高级函数使用：
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
