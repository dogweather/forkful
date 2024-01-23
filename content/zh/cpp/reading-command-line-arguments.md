---
title:                "读取命令行参数"
date:                  2024-01-20T17:55:41.514166-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/reading-command-line-arguments.md"
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
