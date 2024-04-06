---
date: 2024-01-20 17:51:59.288699-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.406614-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) \u5386\u53F2\u4E0A\uFF0C\u6253\u5370\u8C03\u8BD5\
  \u4ECE\u6700\u65E9\u7684\u6253\u5B54\u5361\u7247\u65F6\u4EE3\u5C31\u5F00\u59CB\u4F7F\
  \u7528\u4E86\uFF0C\u7A0B\u5E8F\u5458\u9700\u8981\u76F4\u63A5\u67E5\u770B\u4E2D\u95F4\
  \u7ED3\u679C\u3002`std::cout`\u662FC++\u7684\u6807\u51C6\u8F93\u51FA\u6D41\u5BF9\
  \u8C61\uFF0C\u9002\u5408\u521D\u5B66\u8005\u3002\u66FF\u4EE3\u65B9\u6848\u5305\u62EC\
  \u4F7F\u7528\u66F4\u9AD8\u7EA7\u7684\u8C03\u8BD5\u5DE5\u5177\u5982gdb\u3001LLDB\u6216\
  IDE\u5185\u7F6E\u8C03\u8BD5\u529F\u80FD\uFF0C\u5B83\u4EEC\u53EF\u4EE5\u8BBE\u7F6E\
  \u65AD\u70B9\u3001\u68C0\u67E5\u5185\u5B58\u7B49\uFF0C\u66F4\u7CBE\u7EC6\u5730\u63A7\
  \u5236\u8C03\u8BD5\u8FC7\u7A0B\u3002\u5B9E\u73B0\u4E0A\uFF0C`std::cout`\u4F1A\u53D1\
  \u9001\u6570\u636E\u5230stdout\uFF0C\u8FD9\u901A\u5E38\u4E0E\u63A7\u5236\u53F0\u5173\
  \u8054\u5728\u4E00\u8D77\u3002\u5728\u591A\u7EBF\u7A0B\u73AF\u5883\u4E2D\uFF0C`std::cout`\u662F\
  \u7EBF\u7A0B\u5B89\u5168\u7684\uFF0C\u4F46\u8F93\u51FA\u53EF\u80FD\u4F1A\u56E0\u4E3A\
  \u591A\u4E2A\u7EBF\u7A0B\u540C\u65F6\u8F93\u51FA\u800C\u4EA4\u9519\u51FA\u73B0\u3002"
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
