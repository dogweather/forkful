---
date: 2024-01-20 17:55:41.514166-07:00
description: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u5141\u8BB8\u7A0B\u5E8F\u6839\
  \u636E\u7528\u6237\u8F93\u5165\u8FDB\u884C\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u662F\u4E3A\u4E86\u63D0\u4F9B\u7075\u6D3B\u6027\u548C\u53C2\u6570\u5316\
  \u8FD0\u884C\u9009\u9879\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.929295-06:00'
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u5141\u8BB8\u7A0B\u5E8F\u6839\
  \u636E\u7528\u6237\u8F93\u5165\u8FDB\u884C\u64CD\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u662F\u4E3A\u4E86\u63D0\u4F9B\u7075\u6D3B\u6027\u548C\u53C2\u6570\u5316\
  \u8FD0\u884C\u9009\u9879\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
读取命令行参数允许程序根据用户输入进行操作。程序员这么做是为了提供灵活性和参数化运行选项。

## How to: (如何操作)
```C++
#include <iostream>

int main(int argc, char** argv) {
    std::cout << "你有 " << argc << " 个命令行参数：" << std::endl;
    for (int i = 0; i < argc; ++i) {
        std::cout << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```

运行程序 (比如叫做 `app`) 后，会显示:
```
你有 3 个命令行参数：
0: app
1: 参数1
2: 参数2
```

## Deep Dive (深入了解)
命令行参数自编程之初就存在，在 Unix 系统中尤为常见。C++ 使用 `argc` 表示参数数量，`argv` 是参数值数组。除了`main`函数标准方式，你可以使用库如 `getopt` 在 Unix 或 `Boost.Program_options` 在 C++ 中解析复杂参数。实现细节包括字符串处理和错误检查。

## See Also (另请参考)
- C++ 标准库文档: https://en.cppreference.com/w/cpp/header
- Boost.Program_options 库: https://www.boost.org/doc/libs/release/libs/program_options/
- Unix `getopt`: http://man7.org/linux/man-pages/man3/getopt.3.html
