---
title:    "C: 读取文本文件"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么会读取文本文件？
在C语言编程中，读取文本文件是常见的操作。通过读取文本文件，我们可以轻松地获取文本内容，并在程序中使用它们。这可以帮助我们实现一些有用的功能，比如导入数据或者读取用户设置。因此，学习如何读取文本文件对于提高C语言编程的技能是非常重要的。

## 如何读取文本文件？

```C
#include <stdio.h>

int main()
{
    FILE *file; //指向文件的指针
    char ch;

    file = fopen("sample.txt", "r"); //以只读模式打开文件
    if (file == NULL) //如果文件打开失败
    {
        printf("无法打开文件！\n");
        return 1; //退出程序
    }

    while ((ch = fgetc(file)) != EOF) //循环读取文件内容直到文件结尾
    {
        printf("%c", ch); //打印每个字符
    }

    fclose(file); //关闭文件
    return 0;
}
```

**示例输出：** 如果我们有一个名为"sample.txt"的文本文件，并且其中包含如下内容：

```
Hello, world!
This is a sample text file.
```

上述代码将读取该文本文件，并输出以下内容：

```
Hello, world!
This is a sample text file.
```

现在，你已经学会了如何用C语言来读取文本文件，让我们来深入了解一下。

## 深入了解文本文件的读取

在C语言中，文本文件是以字符流的形式来读取的。在上面的示例中，我们使用`fgetc()`函数来读取文件中的每个字符。此外，你也可以使用`fgets()`函数来读取整行文本。

但是**注意**，在C语言中，文本文件的读取是按照字符来操作的，所以可能会出现一些问题。比如，当文件中包含特殊字符时，可能会出现编码问题。因此，在读取文本文件时，需要特别注意这些细节，并做好相应的处理。

## 参考资料

- [C语言——文件的读写](https://blog.csdn.net/earbao/article/details/87677453)
- [C语言——如何读取文本文件](https://blog.csdn.net/fanstone1995/article/details/100148863)
- [C语言——文本文件的读写](https://www.runoob.com/cprogramming/c-file-io.html)

## 查看也可

- [C语言编程基础知识](https://blog.csdn.net/lwoden/article/details/70298342)
- [C语言——如何处理文本文件中的特殊字符](https://blog.csdn.net/wiatud/article/details/80889846)
- [如何在C语言中进行字符编码转换](https://www.cnblogs.com/phinecos/archive/2012/04/14/2450490.html)