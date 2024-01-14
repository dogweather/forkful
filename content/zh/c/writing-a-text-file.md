---
title:                "C: 编写文本文件"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 为什么要写文本文件？

文本文件被广泛使用来存储和传输数据。它们可以用来保存文本、数字、图像等各种类型的数据。因此，通过编写文本文件，我们可以轻松地存储和共享我们的数据，并且可以在不同的程序之间共享数据。

## 如何编写文本文件

首先，我们需要添加一个头文件`<stdio.h>`，它包含了一些必要的函数和数据类型用于处理输入/输出操作。

在创建文本文件之前，我们需要首先打开一个文件指针。这可以通过`fopen()`函数来完成，它的第一个参数是文件名，第二个参数是打开方式，比如“w”表示写入模式。

接下来，我们可以使用`fprintf()`函数来向文件写入数据。这个函数使用方式类似于`printf()`，只不过它的第一个参数是文件指针，而不是标准输出。

最后，我们需要记得关闭文件指针，使用`fclose()`函数。

下面是一个简单的例子，展示了如何写入文本文件：

```C
#include <stdio.h>

int main() {
    // 打开文件指针
    FILE *fp = fopen("my_file.txt", "w");
    // 向文件写入数据
    fprintf(fp, "Hello, world!");
    // 关闭文件指针
    fclose(fp);
    return 0;
}
```

运行这个程序后，你将会在当前目录下看到一个名为`my_file.txt`的文本文件，里面包含了一行“Hello, world!”的内容。

## 深入了解文本文件

除了上述提到的函数，C语言还提供了一些其他的函数来处理文本文件，比如`fscanf()`用于从文件中读取数据，`fputc()`用于向文件写入单个字符等等。此外，通过使用不同的打开方式，我们还可以控制文件的读写权限，比如“r”表示只读模式，而“a”表示追加模式。

另外，文本文件也可以使用标准输入和输出函数来处理，比如`scanf()`和`printf()`，它们与文件指针无关。但是需要注意的是，这些函数读写的是标准输入输出流，而不是真正的文件。

总的来说，编写文本文件是一项非常有用的技能，它为我们处理各种类型的数据提供了便利。确保熟悉和掌握文本文件的相关函数和打开方式，将有助于提高你的C编程能力。

## 参考链接

- [菜鸟教程 - C语言文件I/O](https://www.runoob.com/cprogramming/c-file-input-output.html)
- [C语言中文网 - 文件操作函数](http://c.biancheng.net/cpp/html/198.html)
- [C语言中文网 - 标准库函数](http://c.biancheng.net/cpp/html/210.html)

# 参见

- [Markdown语法指南 - Markdown基础](https://www.markdown.xyz/basic/)
- [维基百科 - 文本文件](https://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E6%96%87%E4%BB%B6)