---
date: 2024-01-26 04:12:07.107049-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A C++\u672C\u8EAB\u5E76\u4E0D\u5185\u7F6E\
  REPL\u529F\u80FD\uFF0C\u4F46\u50CFCling\u8FD9\u6837\u7684\u5DE5\u5177\u63D0\u4F9B\
  \u4E86\u8FD9\u79CD\u80FD\u529B\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528Cling\u8BA1\
  \u7B97\u4E24\u4E2A\u6570\u7684\u548C\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T21:53:48.405773-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
