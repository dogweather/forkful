---
title:                "使用调试器"
date:                  2024-01-26T03:48:04.960864-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用调试器"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/using-a-debugger.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
使用调试器意味着启动一个工具，让你能够深入运行中的程序内部，以了解实际发生了什么。程序员这样做是为了找到并消除 bugs——那些导致你的代码表现出预期之外的行为或崩溃的讨厌问题。

## 如何操作：
C++ 可以与GDB或Visual Studio调试器等调试器集成。这里有一个使用GDB的简短示例：

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    int c = a / b; // 哎呀，除以零的错误！
    std::cout << c << std::endl;
    return 0;
}

// 编译方式：
// g++ -g -o my_program my_program.cpp

// 使用调试器运行：
// gdb ./my_program
```

一旦你启动了GDB，你就可以设置断点、逐步执行代码、检查变量等等。如果你运行上述代码，应该会看到你的程序因为除以零而崩溃。

## 深入探讨
调试的根源可以追溯到编程的早期，那时候确实需要从硬件中移除虫子（昆虫！）来避免问题。从那以后，调试工具已经发展成为复杂而强大的软件，对开发至关重要。

C++的其它调试工具选择包括LLDB，以及集成在IDE中的调试器，比如Visual Studio、CLion或Eclipse中的调试器。这些现代环境提供了图形界面，使调试变得不那么令人望而生畏。

使用调试器的实现细节通常取决于你的开发环境：

- 命令行调试器（GDB、LLDB）需要熟悉终端命令，并且通常涉及更陡峭的学习曲线。
- 图形调试器通过允许点按和点击交互来设置断点、逐步执行代码和观察变量，简化了过程。

理解你的调试器的能力，如条件断点、监视点或评估表达式，可以显著提升你在诊断问题时的效率。

## 另请参阅
- [GDB 文档](https://www.gnu.org/software/gdb/documentation/)
- [LLDB 命令文档](https://lldb.llvm.org/use/map.html)
- [Visual Studio 调试器教程](https://docs.microsoft.com/en-us/visualstudio/debugger/debugger-feature-tour)
- [使用 CLion 进行调试](https://www.jetbrains.com/help/clion/debugging-code.html)