---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么? | What & Why?

临时文件是在程序运行过程中而产生的临时存储数据的文件。程序员创建它是因为当处理大量数据或者想在重新运行程序时避免数据丢失的情况的时候。

## 如何做 | How to:

创建临时文件的简单示例：
```C
#include <stdio.h>
 
int main()
{
    char tmpname[L_tmpnam];
    char *filename;
    FILE *tmpfp;
 
    filename = tmpnam(tmpname);
    
    printf("Temporary file name is: %s\n", filename);
    tmpfp = fopen(filename, "w+b");
    if (tmpfp) {
        printf("Opened a temporary file OK\n");
        fclose(tmpfp);
        if (remove(filename) == 0)
            printf("Temporary file removed OK\n");
        else
            perror("remove");
    } else {
        perror("tmpfile");
    }
    return 0;
}
```
这个代码会生成临时文件并显示其名字。如果成功，将会显示已经打开和删除临时文件的信息。

## 深入探究 | Deep Dive:

临时文件的创建有些历史背景。早期的程序设计者们发现，当处理大量数据时，使用临时文件可以防止内存溢出，提高程序效率。
除了`tmpnam`之外，`mkstemp`和`tmpfile`也是创建临时文件的常用函数。`tmpnam`生成一个唯一的文件名，`mkstemp`创建一个唯一的临时文件，并返回一个可用于读写的文件描述符。`tmpfile`函数，会创建一个用于更新模式(w+)的临时二进制文件。

具体实现上，临时文件通常在`/tmp`或`/var/tmp`中创建。每个临时文件都有一个唯一的名称，通常是在进程ID后附加随机字符生成的。
使用完毕后，临时文件应该被删除以释放系统资源。

## 参看 | See Also:

C 库函数 tmpnam() - http://www.cplusplus.com/reference/cstdio/tmpnam/
创建临时文件的更多方式 - https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html
在C中实现临时文件 - https://www.tutorialspoint.com/c_standard_library/c_function_tmpnam.htm