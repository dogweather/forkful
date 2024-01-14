---
title:                "C: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么
在进行C语言编程时，有时我们需要临时存储一些数据或创建一个临时文件，这可以帮助我们更有效地处理数据和程序。通过创建临时文件，我们可以暂时保存数据，避免使用硬编码的方式，同时也可以节省内存空间。因此，创建临时文件在编程中是非常有用的。

## 如何
在C语言中，我们可以使用`fopen()`函数来创建一个临时文件，并使用`fprintf()`函数来向文件中写入数据。下面是一个简单的示例代码：

```C
// 打开或创建临时文件
FILE *fp = fopen("temp.txt", "w+"); 

// 向文件中写入数据
fprintf(fp, "这是一条测试数据\n");

// 关闭文件
fclose(fp);
```

运行上述代码后，我们可以在当前工作目录下看到一个名为`temp.txt`的临时文件，其中包含了我们写入的数据。

## 深入探讨
在创建临时文件时，我们可以使用不同的打开模式来决定文件的行为。例如，使用`w+`模式可以创建一个新文件，并且如果文件已存在，则会被截断为空文件。而使用`a+`模式可以在已有文件末尾追加数据。另外，我们还可以使用`tmpfile()`函数来创建一个没有文件名的临时文件，这个临时文件会在关闭时被自动删除。

除了临时文件，我们还可以使用`tmpnam()`函数来生成一个唯一的临时文件名。这在需要临时存储数据但不想创建实际文件时非常有用。

## 参考链接
- [C语言文件操作](https://www.runoob.com/cprogramming/c-file-io.html)
- [创建临时文件与目录](https://blog.csdn.net/Senior_How/article/details/104539087)
- [临时文件的创建与删除](https://www.cnblogs.com/paddix/p/5450607.html)

# 请参阅
- [创建临时文件的C代码示例](https://github.com/Sunfished/creating-temp-file-in-C)
- [C程序设计基础教程](http://c.biancheng.net/c/)