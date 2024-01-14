---
title:                "C: 读取文本文件"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

文本文件是我们在计算机领域经常遇到的一种数据类型。当我们处理文本文件时，可能需要读取它们并从中提取有用的信息。这就是为什么学习如何使用C语言来读取文本文件是一个重要的技能。

## 如何

读取文本文件可以分为几个步骤：

1. 首先，我们需要打开文本文件，可以使用C语言中的`fopen()`函数来实现。

2. 接下来，我们可以使用`fscanf()`函数来读取文件中的数据。这个函数类似于`scanf()`，但是它会从给定的文件中读取数据。

3. 一旦我们读取了需要的数据，我们就可以使用`fclose()`函数来关闭文件。

下面是一个简单的例子，演示如何读取一个文本文件中的数据并打印出来：

```C
#include <stdio.h>

int main() {
    FILE *fp;  // 创建一个指向文件的指针
    char str[255];  // 创建一个字符串存储读取的数据

    // 打开文件
    fp = fopen("example.txt", "r");

    // 从文件中读取数据并打印出来
    fscanf(fp, "%s", str);
    printf("%s", str);

    // 关闭文件
    fclose(fp);

    return 0;
}
```

如果我们有一个名为`example.txt`的文本文件，其中包含内容`Hello World`，那么运行以上程序的输出将是`Hello`。

## 深入了解

读取文本文件可能涉及更复杂的操作，比如处理不同类型的数据、支持多语言字符等。如果想要深入了解读取文本文件的更多技巧和技巧，可以参考C语言的文件输入/输出（I/O）库函数。

## 参考资料

- [C语言文件输入/输出教程](https://www.learn-c.org/en/File_IO)
- [C语言文件I/O函数文档](https://www.tutorialspoint.com/c_standard_library/c_function_fscanf.htm)

## 参见

- [C语言字符串处理教程](https://www.learn-c.org/en/String_Manipulation)
- [C语言常用函数大全](https://www.tutorialspoint.com/c_standard_library/index.htm)