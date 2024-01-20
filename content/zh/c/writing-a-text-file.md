---
title:                "撰写文本文件"
html_title:           "C: 撰写文本文件"
simple_title:         "撰写文本文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

# C语言中的文本文件写入

## 是什么 & 为什么？
文本文件写入是指将文本数据存储到计算机中的过程。程序员经常这样做，因为它可以让他们的程序保存和读取数据，从而让程序更加灵活和实用。

## 如何：
```C
//打开文件
FILE *fp = fopen("example.txt", "w");

//检查文件是否成功打开
if (fp == NULL) {
    printf("无法打开文件\n");
    exit(1);
}

//写入文本数据
fprintf(fp, "这是一行文本数据\n");
fprintf(fp, "%d", 123); //可以写入变量和表达式
fprintf(fp, "\n"); //写入换行符

//关闭文件
fclose(fp);
```

## 深入探讨：
文本文件写入的历史可以追溯到计算机诞生时期，它是最早的数据存储方式之一。除了写入文件外，程序员也可以使用类似的方法读取文件中的文本数据。在C语言中，除了使用文本文件写入外，还有其他一些数据存储方式，比如二进制文件写入。文本文件写入的实现依赖于操作系统和编译器，但是基本原理是相同的。

## 参考资料：
- [C语言官方文档](https://zh.cppreference.com/w/c)提供了关于文件操作的详细说明。