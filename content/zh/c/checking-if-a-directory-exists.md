---
title:                "请登录注释题目检查目录是否存在"
html_title:           "C: 请登录注释题目检查目录是否存在"
simple_title:         "请登录注释题目检查目录是否存在"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

您是否曾经在编写C程序时，需要判断某个目录是否存在？现在，让我们一起来探讨一下为什么需要这样做。

在很多情况下，我们可能需要通过程序来创建、删除、读取或修改一个目录。但是在操作之前，我们需要确认目录是否存在，以免引发报错或者产生其他不必要的问题。因此，检查目录是否存在是一项很重要的功能，相信大家也会经常用到。

## 怎么做

首先，我们需要包含相关的头文件`<sys/stat.h>`和`<unistd.h>`。接着，我们可以使用下面的代码来判断目录是否存在：

```C
char *path = "目录路径";
if (access(path, F_OK) != -1) { //判断目录是否存在
    // 目录存在时的操作
} else {
    // 目录不存在时的操作
}
```

其中，`access()`函数用于检查文件或目录的访问权限，第一个参数为目录路径，第二个参数为操作码，`F_OK`表示检查文件是否存在。如果目录存在，则函数返回0，否则返回-1。

下面我们来看一个完整的例子：

```C
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

int main() {
    // 假设我们要判断是否存在名为 "test" 的目录
    char *path = "test";
    if (access(path, F_OK) != -1) {
        printf("目录 %s 存在。\n", path);
    } else {
        printf("目录 %s 不存在。\n", path);
    }
    return 0;
}
```

输出结果为：目录test存在。

## 深入探讨

在C语言中，检查目录是否存在有几种常见的方法：

1. 使用`access()`函数，如上所述；
2. 使用`stat()`函数，也属于<sys/stat.h>头文件，在目录存在时返回0，不存在时返回-1；
3. 使用`opendir()`函数打开目录，如成功打开，则表明目录存在，失败则表示不存在。

需要注意的是，以上方法均只能用于检查目录是否存在，而不能判断其具体类型（是一个目录、文件还是符号链接），也不能检查目录的读写权限。

## 参考链接

- [C语言检查文件/目录是否存在的几种方法](https://cloud.tencent.com/developer/article/1098992)
- [C语言检查文件/目录是否存在的方法总结](https://www.jianshu.com/p/b28c6509ea50)
- [C语言库函数-access()用法](https://www.runoob.com/cprogramming/c-function-access.html)

## 另请参阅

- [C语言文件操作指南](https://www.runoob.com/cprogramming/c-file-io.html)
- [Linux系统下文件和目录的基础操作](https://www.jianshu.com/p/d4fe9e4ec0d1)