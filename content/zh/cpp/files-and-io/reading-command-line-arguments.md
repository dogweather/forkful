---
date: 2024-01-20 17:55:41.514166-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) \u547D\u4EE4\u884C\u53C2\u6570\u81EA\
  \u7F16\u7A0B\u4E4B\u521D\u5C31\u5B58\u5728\uFF0C\u5728 Unix \u7CFB\u7EDF\u4E2D\u5C24\
  \u4E3A\u5E38\u89C1\u3002C++ \u4F7F\u7528 `argc` \u8868\u793A\u53C2\u6570\u6570\u91CF\
  \uFF0C`argv` \u662F\u53C2\u6570\u503C\u6570\u7EC4\u3002\u9664\u4E86`main`\u51FD\u6570\
  \u6807\u51C6\u65B9\u5F0F\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u5E93\u5982 `getopt`\
  \ \u5728 Unix \u6216 `Boost.Program_options` \u5728 C++\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.336991-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) \u547D\u4EE4\u884C\u53C2\u6570\u81EA\u7F16\u7A0B\
  \u4E4B\u521D\u5C31\u5B58\u5728\uFF0C\u5728 Unix \u7CFB\u7EDF\u4E2D\u5C24\u4E3A\u5E38\
  \u89C1\u3002C++ \u4F7F\u7528 `argc` \u8868\u793A\u53C2\u6570\u6570\u91CF\uFF0C`argv`\
  \ \u662F\u53C2\u6570\u503C\u6570\u7EC4\u3002\u9664\u4E86`main`\u51FD\u6570\u6807\
  \u51C6\u65B9\u5F0F\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u5E93\u5982 `getopt` \u5728\
  \ Unix \u6216 `Boost.Program_options` \u5728 C++ \u4E2D\u89E3\u6790\u590D\u6742\u53C2\
  \u6570\u3002\u5B9E\u73B0\u7EC6\u8282\u5305\u62EC\u5B57\u7B26\u4E32\u5904\u7406\u548C\
  \u9519\u8BEF\u68C0\u67E5\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
