---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
Debug输出是程序在执行过程中产生的运行信息，程序员可以通过这些信息检查和理解代码的运行结果。这个过程可以帮助我们更好地理解代码运行状态，找出并解决问题。

## 具体操作：
在C++中，我们可以使用包含在<iostream>库中的std::cout来进行debug输出。以下是一个基本示例：

```C++
#include <iostream>

int main() {
    int arr[] = {1, 2, 3, 4, 5};

    for (int i : arr) {
        std::cout << "debug: " << i << '\n';
    }

    return 0;
}
```
这段代码将打印数组中的每一个元素，输出结果如下：

```bash
debug: 1
debug: 2
debug: 3
debug: 4
debug: 5
```

## 深入了解：
Debug输出其实是一个在计算机编程早期就已经使用的技术，在那时的硬件条件下这是最有效的错误检查手段。C++语言在设计之初就考虑了debug的需求，因此在其标准库中包含了std::cout。

虽然使用std::cout是最常见的方法，但其它方式例如使用C++标准库中的std::cerr和std::clog也是一个不错的选择，这些方法主要用于错误和日志的输出。

另外，也可以使用像gdb这样的debugger工具来进行debug，这些工具提供了更深入的检查更复杂的错误的可能。

## 另请参阅：
1. C++官方文档: https://en.cppreference.com/w/
2. std::cout用法示例: https://learnxinyminutes.com/docs/c/
3. gdb工具使用教程: https://www.gnu.org/software/gdb/documentation/
4. Debug技巧与工具: https://stackoverflow.com/questions/tagged/debugging