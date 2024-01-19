---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "C++"
category:             "C++"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么和为什么?

启动新项目意味着创建一个全新的编程任务。程序员之所以这么做，是因为这可以让他们构建符合特定需求的应用程序。

## 如何做：

```C++
// 准备一个示例项目：Hello World 程序
#include <iostream>

int main() {
    std::cout << "Hello World!";
    return 0;
}
```
当你运行这个程序时，你会看到如下输出：

```C++
Hello World!
```

## 深入探讨

历史背景：C++首次出现在1985年，由Bjarne Stroustrup开发。从此，许多程序员开始创建新的项目，以适应各种需求。

替代方案：除C++外，你还可以使用Java，Python，Ruby等等来创建新的项目。选择哪种语言取决于你的项目需求。

实施细节：在C++中，新项目通常从包含`main`函数的文件开始。然后，你可以添加更多的.cpp和.h文件，以构建更复杂的项目。你也可以使用诸如Makefile或CMake之类的工具来管理项目构建。

## 还可以参考

1. ["C++ 参考"](http://www.cplusplus.com/)
2. ["C++ 标准库"](https://zh.cppreference.com/)
3. ["C++ 教程"](https://www.runoob.com/cplusplus/cpp-tutorial.html)