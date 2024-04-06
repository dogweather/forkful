---
date: 2024-01-26 00:50:04.676265-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A C++ \u81EA\u65E9\u671F\u8D77\u5C31\u6709\
  \u9519\u8BEF\u5904\u7406\u80FD\u529B\u3002\u6700\u57FA\u672C\u7684\u5F62\u5F0F\u662F\
  \u68C0\u67E5\u8FD4\u56DE\u503C\u3002\u5982\u679C\u60A8\u7ECF\u9A8C\u4E30\u5BCC\uFF0C\
  \u60A8\u4F1A\u8BB0\u5F97\u6807\u51C6\u524D\u7684\u65E5\u5B50\uFF1A\u6709\u7C7B\u7684\
  C\u8BED\u8A00\u548C\u624B\u52A8\u9519\u8BEF\u68C0\u67E5\u3002 \u7136\u540EC++\u5F15\
  \u5165\u4E86\u5F02\u5E38\uFF0C\u4E3A\u6211\u4EEC\u63D0\u4F9B\u4E86\u4E00\u79CD\u7ED3\
  \u6784\u5316\u7684\u65B9\u5F0F\u6765\u5904\u7406\u610F\u5916\u95EE\u9898\u3002\u7528\
  \ `throw` \u629B\u51FA\u5F02\u5E38\uFF0C\u7528 `try/catch` \u6355\u83B7\u5F02\u5E38\
  \u3002\u2026"
lastmod: '2024-04-05T22:51:01.327512-06:00'
model: gpt-4-1106-preview
summary: "\u7136\u540EC++\u5F15\u5165\u4E86\u5F02\u5E38\uFF0C\u4E3A\u6211\u4EEC\u63D0\
  \u4F9B\u4E86\u4E00\u79CD\u7ED3\u6784\u5316\u7684\u65B9\u5F0F\u6765\u5904\u7406\u610F\
  \u5916\u95EE\u9898\u3002\u7528 `throw` \u629B\u51FA\u5F02\u5E38\uFF0C\u7528 `try/catch`\
  \ \u6355\u83B7\u5F02\u5E38\u3002"
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 如何操作：
以下是一个基本的 try-catch 块来处理异常：

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("哎呀！出错了。");
    } catch (const std::exception& e) {
        std::cerr << "错误：" << e.what() << std::endl;
    }
    return 0;
}
```

示例输出：
```
错误：哎呀！出错了。
```

## 深入探讨
C++ 自早期起就有错误处理能力。最基本的形式是检查返回值。如果您经验丰富，您会记得标准前的日子：有类的C语言和手动错误检查。

然后C++引入了异常，为我们提供了一种结构化的方式来处理意外问题。用 `throw` 抛出异常，用 `try/catch` 捕获异常。

常见的两种错误是：逻辑错误，比如错误的计算，和运行时错误，比如访问无效的内存地址。对于运行时错误，异常处理是理想选择。对于逻辑错误，通常最好使用断言或错误代码。

关于异常与错误代码之间的辩论一直在进行。异常可能会更慢，并可能导致控制流程复杂化。而错误代码虽然更快，却可能使代码变得杂乱并更难以维护。这是一个权衡，所以了解您的使用案例是关键。

C++17 引入了 `std::optional` 和 `std::variant`，这些是异常的替代品。对于可能会或可能不会返回有效结果的函数而言，这些非常有用。

异常安全性可能是另一个头疼的问题。这是关于您的代码尽管有异常也能提供的保证。有三个级别：基本、强和不抛异常。越多的保证，您的代码可能就越复杂。

最后的想法 - 错误处理既是艺术也是科学。它塑造了您的应用程序在野外的生存能力。不要过度使用异常。目标是可读性好、易于维护的代码。

## 另请参阅
- [cppreference 关于异常处理](https://zh.cppreference.com/w/cpp/language/exceptions)
- [Bjarne Stroustrup 对错误处理的看法](http://www.stroustrup.com/except.pdf)
- [C++ 核心指南中关于异常的部分](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)
