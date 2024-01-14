---
title:                "C: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

读取文本文件是C语言编程中一个基本的技巧。它允许程序员读取和操作外部文件中的数据，从而使程序更加灵活和强大。不管你是正在学习C语言还是想要提高你的编程技巧，掌握读取文本文件的方法都是非常重要的。

## 如何做

要读取文本文件，需要使用C语言中的标准库函数fopen()和fscanf()。下面是一个简单的示例代码，展示如何使用这两个函数来读取一个名为"input.txt"的文本文件，并将其中的内容打印出来。

```C
#include <stdio.h>

int main() {
    FILE *fp; // 定义文件指针
    char name[20]; // 用于存储文件中的字符串
    int age; // 用于存储文件中的整数

    // 打开文件，如果失败则打印错误信息并退出程序
    if ((fp = fopen("input.txt", "r")) == NULL) {
        printf("Error opening file.");
        return 1;
    }

    // 从文件中读取格式化的数据，并将其存储到相应的变量中
    fscanf(fp, "%s %d", name, &age);

    // 关闭文件
    fclose(fp);

    // 打印读取的数据
    printf("Name: %s\n", name);
    printf("Age: %d\n", age);

    return 0;
}
```

假设输入文件内容为："John 25"，则上述代码的输出为：

```
Name: John
Age: 25
```

## 深入探究

在继续深入了解读取文本文件之前，让我们先了解一下文件指针的概念。在上面的示例代码中，我们定义了一个FILE类型的指针变量fp。这个指针指向一个打开的文件，允许我们使用其他函数来读取文件中的数据。在使用完毕后，需要使用fclose()函数来关闭文件。

另外，fscanf()函数的格式化字符串参数可以根据文件中数据的格式进行自定义。格式化字符串中的%符号指示了要读取的数据类型，如%c表示一个字符，%d表示一个整数，%s表示一个字符串，%f表示一个浮点数等等。

## 参考链接

- [C语言文件I/O](https://www.runoob.com/cprogramming/c-file-io.html)
- [fopen()函数文档](https://zh.cppreference.com/w/c/io/fopen)
- [fscanf()函数文档](https://zh.cppreference.com/w/c/io/fscanf)

## 参见

[如何写入文本文件](https://www.example.com/how-to-write-to-a-text-file-in-c)