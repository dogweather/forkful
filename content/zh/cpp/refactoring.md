---
title:                "代码重构"
date:                  2024-01-26T01:17:23.240543-07:00
model:                 gpt-4-0125-preview
simple_title:         "代码重构"
programming_language: "C++"
category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/refactoring.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

重构是改变计算机程序内部结构而不改变其外部行为的过程。程序员这样做是为了清理他们的代码，使其更易于理解、维护和扩展。

## 如何进行：

想象一下，你有一个函数做的事情有点多，如同这个笨重的方法，既初始化了一个对象，也执行了日志记录：

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // 初始化逻辑
        // ...

        // 详细日志记录
        if (verbose) {
            std::cout << "Widget initialized!" << std::endl;
        }
    }
};

// 使用：
Widget w;
w.init(true);
```

输出：
```
Widget initialized!
```

将其重构为更清晰、更专注的方法可能像这样：

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // 仅包含初始化逻辑
        // ...
    }

    void logInitialization() const {
        std::cout << "Widget initialized!" << std::endl;
    }
};

// 使用：
Widget w;
w.init();
w.logInitialization();
```

这次改变并没有改变程序做什么，但使得 `Widget` 类更加模块化，其使用也更清晰。

## 深入探讨

我们今天所知的重构概念，起始于1980年代Smalltalk编程社区，而后由Martin Fowler在1999年出版的书籍《重构：改善既有代码的设计》中大力推广。今天，重构是现代软件开发的核心部分，被集成到各种开发方法论中，如敏捷和TDD（测试驱动开发）。

当我们讨论重构的替代品时，我们会进入重写或重新设计的领域。重构是策略性和增量的，而重写可能会抛弃现有代码，转而采用新的解决方案。与此同时，重新设计可能会涉及更重大的变化，包括改变功能，这对于纯粹的重构来说是非目标的。

关于重构的实现细节可以非常细致。有很多“代码异味”可能促使重构，如方法过长、类过大或代码重复。存在一些自动化工具可以协助重构，例如C++的“Clang-Tidy”，它可以发现问题甚至应用一些修复。

此外，重构需要一套稳固的测试以确保功能保持不变。没有测试，你基本上是盲飞，冒着回归的风险。

## 参见

如果要深入了解重构并查看更多示例，你可能想要查看：

- Martin Fowler的经典文本《重构：改善既有代码的设计》，了解基础思想和策略。
- 查阅 https://clang.llvm.org/extra/clang-tidy/ 的 `Clang-Tidy` 文档，了解在C++中自动化重构支持。
- Michael Feathers的《与遗留代码高效合作》，提供了在不完美的现有代码库中安全进行重构的技术。
