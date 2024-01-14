---
title:                "C: 向標準錯誤輸出"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

你为什么要向标准错误编写

## 为什么

写入标准错误是C程序中一种重要的技术。当程序需要向用户显示错误或调试信息时，这种写入形式非常有用。通过向标准错误编写，我们可以确保这些信息会显示在用户的屏幕上，而不会干扰到程序的正常输出。

## 如何进行

为了向标准错误编写，我们需要使用`fprintf`函数，并将标准错误流`stderr`作为参数。以下是一个简单的示例代码，演示如何向标准错误编写一条消息：

```C
fprintf(stderr, "这是一条错误信息\n");
```

运行此程序会将消息打印到屏幕上，类似于这样：

```
这是一条错误信息
```

我们还可以使用格式化字符串来添加变量或表达式的值。例如，下面的代码会在错误信息中显示文件名、行号以及一个变量的值：

```C
fprintf(stderr, "文件 %s 中的第 %d 行出现错误。变量值为：%d\n", __FILE__, __LINE__, some_variable);
```

输出效果如下：

```
文件 main.c 中的第 10 行出现错误。变量值为：42
```

## 深入了解

除了使用`fprintf`函数，我们还可以使用`fputc`和`fputs`等函数向标准错误编写。同时，C语言为我们提供了一些方便的宏定义，例如`__FILE__`和`__LINE__`，可以帮助我们打印出当前文件名和行号。此外，在多线程程序中，我们需要注意确保多个线程不会同时向标准错误编写，否则可能会导致信息的混乱。

## 参考链接

- [C语言手册 - fprintf函数](https://www.runoob.com/cprogramming/c-function-fprintf.html)
- [C语言手册 - 标准错误流](https://www.runoob.com/cprogramming/c-standard-error.html)
- [C语言手册 - 多线程编程](https://www.runoob.com/cprogramming/c-multithreading.html)

## 另请参阅

[C语言基础知识](https://www.runoob.com/cprogramming/c-tutorial.html)