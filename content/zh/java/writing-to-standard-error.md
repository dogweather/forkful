---
title:                "写入标准错误"
html_title:           "Java: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

当人们编写Java程序时，经常会遇到错误和异常。通常情况下，这些错误和异常会被记录到日志文件中。然而，有时候我们也需要将它们输出到标准错误流（standard error）中。这样做的主要原因是可以及时地收集程序的错误信息，在调试过程中更加方便定位和解决问题。

## 如何操作

要将错误信息输出到标准错误流中，可以使用System类中的`err`对象，继而使用`println`方法。下面是一个简单的例子：

```Java
System.err.println("这是一个错误信息");
```

输出的结果如下：

```
这是一个错误信息
```

## 深入探讨

在深入了解如何输出到标准错误流之前，首先需要了解标准流的概念。标准流是指程序与环境之间的数据传输通道，Java程序中主要涉及到的有标准输入流（standard input）、标准输出流（standard output）和标准错误流（standard error），它们分别对应着`System.in`、`System.out`和`System.err`。其中标准错误流专门用于输出错误信息，而标准输出流则用于输出一般的信息。通常情况下，错误信息会被红色标识，有助于区分和定位问题。

## 参考链接

- [Java System类文档](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/lang/System.html)
- [了解Java标准流](https://www.geeksforgeeks.org/understanding-javas-standard-input-output/)
- [如何处理Java中的错误和异常](https://www.baeldung.com/java-exceptions)
- [学习Java的101个代码实例](https://java2blog.com/100-java-projects-with-source-code/)