---
title:                "C: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么
当我们写程序时，经常会遇到一些错误或者bug。有时候，我们需要在程序中加入一些debug输出来帮助我们找到问题的所在。这样做可以让我们更容易地定位错误，从而节省调试的时间。

## 如何做
我们可以使用C语言的printf函数来打印debug输出。例如，我们可以在程序中加入以下代码来打印出"Hello World!"：

```C
#include <stdio.h>

int main() {
    printf("Hello World!");
    return 0;
}
```

代码块中的内容将会在程序执行时打印出来，让我们可以验证程序的运行状态。如果我们不想在发布版本中出现这些debug输出，我们可以使用宏定义来控制打印语句的执行。

## 深入探讨
除了使用printf函数，我们还可以使用类似于GDB这样的调试器来帮助我们定位错误。调试器能够让我们在程序运行时检查变量的值，从而更直观地了解程序的执行情况。除此之外，调试器还有很多其他强大的功能，值得我们去深入学习和探索。

## 参考链接
- [C语言中的调试技巧](https://www.runoob.com/w3cnote/c-debugging-skills.html)
- [GCC调试器GDB入门教程](https://www.ibm.com/developerworks/cn/linux/l-cn-gdb/)
- [使用printf进行调试的一些技巧](https://www.linux.com/news/using-printf-debug-techniques-gdb/)