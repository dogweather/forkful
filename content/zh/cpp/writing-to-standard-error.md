---
title:                "标准错误写入"
html_title:           "C++: 标准错误写入"
simple_title:         "标准错误写入"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么是标准错误，为什么程序员要用它？
标准错误是指C++中的一个输出流（ostream），专门用来显示程序运行过程中的错误信息。由于在程序中可能会发生各种问题，程序员需要通过标准错误来定位和解决错误，使得程序更加稳定可靠。

## 如何使用标准错误：
使用标准错误非常简单，只需要调用标准库中的std::cerr，然后将需要输出的错误信息传递给它即可。以下是一个示例代码和输出结果：

```C++
#include <iostream>

int main() {
    std::cerr << "错误信息：无法打开文件！" << std::endl;
    return 1;
}
```

输出结果：

```
错误信息：无法打开文件！
```

## 深入了解：
标准错误作为C++中的一个标准流，与标准输出（std::cout）和标准输入（std::cin）一样，都是由C++标准库提供的。在早期的C语言中，程序员通常使用printf函数来输出错误信息，但它并不提供任何错误检查功能。而标准错误能够提供更多的帮助，如定位错误发生的位置。除了标准错误，程序员还可以使用其他一些外部库来处理错误信息，如Boost中的Boost.Exception库。

## 参考资料：
- [C++标准库参考手册](https://cppreference.com/)
- [Boost.Exception](https://www.boost.org/doc/libs/1_75_0/libs/exception/doc/index.html)