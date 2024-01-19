---
title:                "检查目录是否存在"
html_title:           "C: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 究竟是什么&为什么?

检查目录是否存在是一种常见的编程需求，可以保证你的程序读取到正确的文件路径，避免出现文件不存在的错误。编程者进行目录检查以保障程序运行的稳定和兼容性。

## 如何操作:

以下是在C言语中实现目录检查的代码示例：

```C
#include <stdio.h>
#include <sys/stat.h>

int main() {
   struct stat st = {0};

   if (stat("/path/to/dir", &st) == -1) {
       printf("目录不存在\n");
   } else {
       printf("目录存在\n");
   }

   return 0;
}
```

运行以上代码，如果目录存在，将输出“目录存在”，如果目录不存在，则输出“目录不存在”。

## 深入探究:

在早期的C版本中，我们需要通过尝试打开目录文件来检查目录是否存在，这种方法并不高效。现在，我们使用stat这个函数，可以更便利、快速地完成这个任务。

另外，还有其他替代方案如使用`access`或`opendir`函数。但这些方法各有利弊，`stat`功能更全面，除了检查目录是否存在，还可以提供目录的信息，如大小，创建时间等。但需要注意的是，以上所有函数在不同操作系统下可能存在差异，所以在具体应用时需要考虑到平台兼容性。

## 查看相关:

以下是一些有关C语言和目录操作的相关资源：

- C语言教程: [https://www.learn-c.org/](https://www.learn-c.org/)
- Linux stat函数详解: [https://man7.org/linux/man-pages/man2/stat.2.html](https://man7.org/linux/man-pages/man2/stat.2.html)
- 关于目录操作的更多信息: [https://pubs.opengroup.org/onlinepubs/009695399/functions/directory.html](https://pubs.opengroup.org/onlinepubs/009695399/functions/directory.html)