---
title:    "C: 检查文件夹是否存在"
keywords: ["C"]
---

{{< edit_this_page >}}

# 为什么要检查目录是否存在

在C编程中，经常会涉及到对目录进行读取、修改或创建操作。但是在进行这些操作之前，我们需要确认目录是否存在。这样可以避免在程序运行时出现意外的错误，提高代码的可靠性和稳定性。

# 如何检查目录是否存在

在C语言中，我们可以使用```access()```函数来检查目录是否存在。该函数的原型为：
```
int access(const char *path, int mode);
```
其中，```path```参数是要检查的目录路径，```mode```参数用于指定检查权限的方式。如果目录存在，则该函数会返回0，否则会返回-1。因此，我们可以通过判断返回值来确定目录是否存在。

下面是一个代码示例，演示了如何使用```access()```函数来检查目录是否存在：
```
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main() {
    char *path = "/home/user/Documents";
    int result = access(path, F_OK); // F_OK表示检查目录是否存在的权限
    if (result == 0) {
        printf("目录存在。\n");
    } else {
        printf("目录不存在。\n");
    }
    return 0;
}
```
运行结果为：
```
目录存在。
```

# 深入了解检查目录是否存在

在C语言中，还有其他一些函数可以用来检查目录是否存在，比如```stat()```函数和```opendir()```函数。```stat()```函数可以获取文件或目录的相关信息，如果目录不存在，则会出现错误。```opendir()```函数可以打开目录，如果目录不存在，则会返回一个空指针。

此外，我们还可以通过使用系统命令```mkdir```来创建目录，但是在使用该命令前，也需要先检查目录是否存在，以防止重复创建。

总而言之，检查目录是否存在是C编程中的一个重要步骤，可以帮助我们避免一些不必要的错误和问题。

# 参考链接

- [C语言文档 - access()函数](https://zh.cppreference.com/w/c/io/access)
- [C语言教程 - 目录处理](https://www.runoob.com/cprogramming/c-tutorial-file-directory.html)
- [Linux命令大全 - mkdir命令](https://www.runoob.com/linux/linux-comm-mkdir.html)