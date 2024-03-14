---
date: 2024-01-26 04:12:07.107049-07:00
description: "REPL\uFF08\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370-\u5FAA\u73AF\uFF09\u662F\
  \u4E00\u4E2A\u7B80\u5355\u7684\u4EA4\u4E92\u5F0F\u7F16\u7A0B\u73AF\u5883\u3002\u7A0B\
  \u5E8F\u5458\u4F7F\u7528\u5B83\u8FDB\u884C\u5B9E\u65F6\u8BED\u8A00\u5B9E\u9A8C\u3001\
  \u5FEB\u901F\u4EFB\u52A1\uFF0C\u6216\u5728\u4E0D\u9700\u8981\u521B\u5EFA\u5B8C\u6574\
  \u5E94\u7528\u7A0B\u5E8F\u7684\u60C5\u51B5\u4E0B\u7406\u89E3\u65B0\u6982\u5FF5\u3002"
lastmod: '2024-03-13T22:44:48.111925-06:00'
model: gpt-4-0125-preview
summary: "REPL\uFF08\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370-\u5FAA\u73AF\uFF09\u662F\
  \u4E00\u4E2A\u7B80\u5355\u7684\u4EA4\u4E92\u5F0F\u7F16\u7A0B\u73AF\u5883\u3002\u7A0B\
  \u5E8F\u5458\u4F7F\u7528\u5B83\u8FDB\u884C\u5B9E\u65F6\u8BED\u8A00\u5B9E\u9A8C\u3001\
  \u5FEB\u901F\u4EFB\u52A1\uFF0C\u6216\u5728\u4E0D\u9700\u8981\u521B\u5EFA\u5B8C\u6574\
  \u5E94\u7528\u7A0B\u5E8F\u7684\u60C5\u51B5\u4E0B\u7406\u89E3\u65B0\u6982\u5FF5\u3002"
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
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
