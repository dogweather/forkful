---
date: 2024-01-20 17:51:59.288699-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.112935-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6253\u5370\u8C03\u8BD5\u4FE1\u606F"
weight: 33
---

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
