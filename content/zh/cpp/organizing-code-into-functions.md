---
title:                "将代码组织成函数"
aliases:
- zh/cpp/organizing-code-into-functions.md
date:                  2024-01-26T01:09:38.566496-07:00
model:                 gpt-4-1106-preview
simple_title:         "将代码组织成函数"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 什么以及为什么?
将代码分解成函数意味着将您的代码分割成更小、可重用的块。我们这样做是为了避免重复，让代码更易读，并简化调试和测试。组织良好的函数就像拥有一个装满整齐标记过的工具的盒子，随时准备使用和分享。

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
