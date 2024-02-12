---
title:                "开始一个新项目"
aliases: - /zh/cpp/starting-a-new-project.md
date:                  2024-01-20T18:02:57.097518-07:00
model:                 gpt-4-1106-preview
simple_title:         "开始一个新项目"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
开始新项目就是创建一份全新的代码框架。程序员这么做为了解决新问题，实验创新的想法，或是学习新技能。

## How to: 如何操作
```C++
#include <iostream>

int main() {
    std::cout << "Hello, New Project!" << std::endl;
    return 0;
}
```
输出：
```
Hello, New Project!
```

## Deep Dive 深入探究
当我们谈论新项目，我们常常想到的是使用 `git` 初始化一个仓库或是在IDE中创建新工程。在C++领域，新项目意味着开始构思和编码从类设计到错误处理的一切。

历史背景：C++自1985年诞生以来, 它一直在不断发展。每个新的C++标准都给项目开始带来了新工具和特性。

替代方案：现在有很多现代化的工具和库，比如CMake构建系统，可以帮助你更高效地开始新项目。

实施细节：在项目起步时，你得考虑编码规范，项目结构和依赖管理。选择正确的编译器和标准库对项目后续发展至关重要。

## See Also 另请参阅
- C++官方文档 [cplusplus.com](https://www.cplusplus.com/)
- GitHub上的C++项目模板 [GitHub - C++](https://github.com/search?q=C%2B%2B+starter+template)
- CMake官方教程 [CMake Tutorial](https://cmake.org/cmake/help/latest/guide/tutorial/index.html)
