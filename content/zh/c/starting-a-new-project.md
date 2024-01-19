---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么和为什么?

开始一个新项目意味着创建一个新的程序或应用。程序员之所以这样做，是因为这是表达创意，解决问题，或者提供用户需要的服务的一种方式。

## 如何做：

下面是如何在 C 语言中创建一个新项目的例子。
我们将创建一个简单的 "Hello, World!" 程序。

```C
#include <stdio.h>

int main() {
   // printf() 显示Hello, World!
   printf("Hello, World!");
   return 0;
}
```

如果一切正确，运行这段代码将在控制台上飘出"Hello, World!"。

## 深度探讨：

**历史背景**：C 语言创建于 1970 年代初，主要用于创建操作系统，尤其是 Unix 操作系统。在那之后，它已经成为了开发新项目的一种流行方式。

**替代方案**：除了 C 语言，也有一些其他的选择可以创建新的项目。例如，Java，Python，和 JavaScript 等语言都可以用于开发各种各样的项目。

**实现细节**：在 C 语言中，每个项目都从一个叫做 main 的函数开始。这个函数是程序运行的起点，它由操作系统在程序启动时调用。

## 延伸阅读：

1. "C programming for beginners" : https://www.learncpp.com/
2. "The C Programming Language" (book by Brian Kernighan and Dennis Ritchie): http://www.amazon.com
3. "Learn C" (freeCodeCamp Tractical Tutorial): https://www.freecodecamp.org/news/
4. "C Programming" (Tutorials Point): https://tutorialspoint.com/