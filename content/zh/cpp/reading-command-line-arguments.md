---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何为和为何要用？(What & Why?)

命令行参数是我们在运行程序时传递给主函数的参数。对开发人员来说，这意味着能灵活地使用不同的输入来改变程序行为。

## 怎么做 (How to)

在C++中，我们这样读取命令行参数：

```C++
#include <iostream>

int main(int argc, char *argv[]) {
    for(int i = 0; i < argc; i++) {
        std::cout << "Arg " << i << ": " << argv[i] << std::endl;
    }

    return 0;
}
```

假设你的程序名为`myProgram`，你使用命令 `./myProgram Hello C++` 运行，则你会得到下面的输出： 

```
Arg 0: ./myProgram
Arg 1: Hello
Arg 2: C++
```

## 深度挖掘 (Deep Dive)

命令行参数的历史可以追溯到C和Unix的早期日子。它们为了最大化灵活性和控制力，特意允许这种参数传递方式。

一种可选的处理命令行参数方式是使用库，例如 `Boost.Program_options`。这些库通常提供了丰富且强大的特性来解析和校验命令行参数。

在实现级别，`argc` 和 `argv` 是由操作系统在程序启动时设置的，您可以选择是否使用它们，依赖于您的实际需求。

## 另类查阅 (See Also)

1. [C++ 参数解析库 - Boost.Program_options 文档](https://www.boost.org/doc/libs/1_76_0/doc/html/program_options.html).
2. [C++ 命令行参数 - cplusplus.com 教程](http://www.cplusplus.com/articles/DEN36Up4/).
3. [ISO C++](https://isocpp.org/) - 了解C++的最新标准和最佳实践。