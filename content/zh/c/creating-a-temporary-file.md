---
title:                "C: 创建临时文件"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

有时，编程过程中我们需要创建临时文件来存储临时数据，而不是直接操作主要文件。这样可以避免意外覆盖或损坏主要文件，并且在程序执行完毕后，临时文件会自动删除，保持电脑整洁。

## 创建临时文件的方法

我们可以使用C语言中的`tmpfile()`函数来创建临时文件。下面是一个简单的例子：

```C
FILE *fp; //声明文件指针
char data[10] = "Hello"; //创建需要写入临时文件的数据
fp = tmpfile(); //使用tmpfile()函数创建临时文件
fprintf(fp, "%s", data); //将数据写入临时文件中
```

运行程序后，我们可以在临时文件中找到存储的数据。临时文件的名称将在运行时自动生成，我们可以通过使用`tmpnam()`函数获取临时文件的完整路径和名称。

```C
char* filename = tmpnam(NULL); //使用tmpnam()函数获取临时文件路径和名称
printf("临时文件路径和名称：%s", filename); 
```

## 深入了解临时文件

除了`tmpfile()`和`tmpnam()`函数，C语言还提供了其他一些函数来操作临时文件，例如`tmpfile_s()`函数和`tempnam()`函数。这些函数的使用方法和效果略有不同，但都能实现创建临时文件的功能。需要注意的是，临时文件会在程序结束时自动删除，但我们也可以使用`fclose()`函数来手动关闭并删除临时文件。

## 参考链接

- [C语言临时文件教程](https://www.runoob.com/cprogramming/c-function-tmpfile.html)
- [临时文件与假文件的区别](https://zhidao.baidu.com/question/1751853642630800527.html)
- [tmpfile()函数文档](http://www.cplusplus.com/reference/cstdio/tmpfile/)