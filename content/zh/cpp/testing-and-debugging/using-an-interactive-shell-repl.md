---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:12:07.107049-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么与为什么？
REPL（读取-求值-打印-循环）是一个简单的交互式编程环境。程序员使用它进行实时语言实验、快速任务，或在不需要创建完整应用程序的情况下理解新概念。

## 如何操作：
C++本身并不内置REPL功能，但像Cling这样的工具提供了这种能力。以下是如何使用Cling计算两个数的和的方法：

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "The sum is: " << a + b << std::endl;
    return 0;
}

// 输出：
// The sum is: 12
```

启动Cling并逐行输入代码，观察每个命令后的输出。这是即时反馈，无需编译。

## 深入探讨
REPL在像Python或Lisp这样的语言中很常见，自1960年代以来就已经存在。对于C++这样的编译语言，这个概念并不自然贴合，这就是像Cling这样的工具存在的原因——它们可以即时解释C++。其他替代方案包括在线编译器或传统编译的小型测试程序。Cling建立在LLVM和Clang之上，为C++以解释方式使用提供了桥梁。

## 另请参阅
- [Cling](https://root.cern/cling/)：一个交互式C++解释器，建立在LLVM和Clang库的顶部。
- [Jupyter Notebooks](https://jupyter.org/)：提供一个在笔记本环境中的交互式shell，通过xeus-cling内核支持C++。
- [LLVM](https://llvm.org/)：一套模块化且可重用的编译器和工具链技术集合，Cling在此基础上构建。
