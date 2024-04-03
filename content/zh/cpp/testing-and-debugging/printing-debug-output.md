---
date: 2024-01-20 17:51:59.288699-07:00
description: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u4EE3\u7801\u4E2D\u63D2\u5165\
  \u7279\u6B8A\u8BED\u53E5\uFF0C\u4EE5\u5728\u63A7\u5236\u53F0\u663E\u793A\u53D8\u91CF\
  \u548C\u7A0B\u5E8F\u72B6\u6001\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u6765\u8DDF\u8E2A\u95EE\u9898\u3001\u7406\u89E3\u7A0B\u5E8F\u6D41\u7A0B\u548C\u9A8C\
  \u8BC1\u4EE3\u7801\u884C\u4E3A\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.112935-06:00'
model: gpt-4-1106-preview
summary: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u4EE3\u7801\u4E2D\u63D2\u5165\
  \u7279\u6B8A\u8BED\u53E5\uFF0C\u4EE5\u5728\u63A7\u5236\u53F0\u663E\u793A\u53D8\u91CF\
  \u548C\u7A0B\u5E8F\u72B6\u6001\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u6765\u8DDF\u8E2A\u95EE\u9898\u3001\u7406\u89E3\u7A0B\u5E8F\u6D41\u7A0B\u548C\u9A8C\
  \u8BC1\u4EE3\u7801\u884C\u4E3A\u3002."
title: "\u6253\u5370\u8C03\u8BD5\u4FE1\u606F"
weight: 33
---

## What & Why? (是什么？为什么？)

打印调试输出是代码中插入特殊语句，以在控制台显示变量和程序状态信息。程序员这样做来跟踪问题、理解程序流程和验证代码行为。

## How to: (如何操作)

```C++
#include <iostream>

int main() {
    int sum = 0;
    for(int i = 0; i < 5; ++i) {
        sum += i;
        // 打印调试信息
        std::cout << "i: " << i << ", sum: " << sum << std::endl;
    }
}
```

输出：
```
i: 0, sum: 0
i: 1, sum: 1
i: 2, sum: 3
i: 3, sum: 6
i: 4, sum: 10
```

## Deep Dive (深入探索)

历史上，打印调试从最早的打孔卡片时代就开始使用了，程序员需要直接查看中间结果。`std::cout`是C++的标准输出流对象，适合初学者。替代方案包括使用更高级的调试工具如gdb、LLDB或IDE内置调试功能，它们可以设置断点、检查内存等，更精细地控制调试过程。实现上，`std::cout`会发送数据到stdout，这通常与控制台关联在一起。在多线程环境中，`std::cout`是线程安全的，但输出可能会因为多个线程同时输出而交错出现。

## See Also (另见)

- [C++ Reference: std::cout](http://www.cplusplus.com/reference/iostream/cout/)
- [C++ Debugging with gdb](https://www.gnu.org/software/gdb/documentation/)
- [Effective Modern C++ by Scott Meyers](https://www.oreilly.com/library/view/effective-modern-c/9781491908419/)
