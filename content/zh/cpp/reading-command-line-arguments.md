---
title:                "读取命令行参数"
html_title:           "C++: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 概要
在编程中，读取命令行参数是一项常见的任务。它允许程序获取用户在命令行中输入的信息，从而为程序提供更多的灵活性和交互性。

## 什么是读取命令行参数及为什么要这么做
读取命令行参数就是在程序运行时获取用户在命令行中输入的信息。它可以让程序在运行时根据用户的输入做出不同的处理，从而增加程序的灵活性和交互性。例如，在一个文本编辑程序中，用户可以在命令行中指定要打开的文件名，从而让程序直接打开用户想要编辑的文件。

## 如何做
下面是一个使用C++语言读取命令行参数的示例:

```C++
#include <iostream>
#include <string>
using namespace std;

int main(int argc, char* argv[]) {

    // 打印命令行参数的数量
    cout << "命令行参数数量:" << argc << endl;

    // 打印每个命令行参数
    for (int i = 0; i < argc; i++) {
        cout << "参数 " << i << ": " << argv[i] << endl;
    }

    return 0;
}
```

运行上面的代码，输入命令行参数，例如 `./program hello world`，程序将会输出:

```
命令行参数数量: 3
参数 0: ./program
参数 1: hello
参数 2: world
```

这里的 `argc` 表示命令行参数的数量，`argv[]` 代表一个字符串数组，其中每个元素都是一个命令行参数。

## 深入挖掘
读取命令行参数的功能可以追溯到命令行界面的早期操作系统。在早期，用户需要手动输入所有的命令和参数，没有图形界面提供点击和拖拽的便捷方式。如今，读取命令行参数的功能可以被其他工具和库所取代，例如读取配置文件等。但是，直接从命令行中获取参数仍然是一种简单而有效的方法。

## 另请参阅
- [命令行参数 - 维基百科](https://zh.wikipedia.org/wiki/%E5%91%BD%E4%BB%A4%E8%A1%8C%E5%8F%82%E6%95%B0)
- [C++命令行参数教程- Cprogramming.com](https://www.cprogrammin