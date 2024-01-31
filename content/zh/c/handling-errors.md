---
title:                "处理错误"
date:                  2024-01-26T00:49:25.327905-07:00
model:                 gpt-4-1106-preview
simple_title:         "处理错误"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/handling-errors.md"
---

{{< edit_this_page >}}

## 何为错误处理？为何要进行错误处理？
在C语言中处理错误涉及到预见未知的情况。它可以防止程序在遇到问题时走火入魔。程序员进行错误处理是为了优雅地处理错误并保持代码的可靠性。

## 如何处理：

我们来看看如何在C语言中做到这一点：

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("nonexistentfile.txt", "r");
    if (fp == NULL) {
        perror("打开文件错误");
        return EXIT_FAILURE;
    }
    // 对文件进行一些处理
    fclose(fp);
    return EXIT_SUCCESS;
}
```

当文件不存在时的样例输出：
```
打开文件错误：没有那个文件或目录
```

## 深入了解

在C语言初期，错误处理方式非常基础——大多通过返回代码和手动检查。引入了`errno`这个全局变量，当函数执行失败时会更新这个变量。不过，`errno`自身并不是线程安全的，因此后来引入了`strerror`和`perror`函数用于更好的错误报告。

有其他选择吗？现代C不仅限于`errno`。还有setjmp和longjmp可以在灾难来临时用于非本地跳转。有些人喜欢定义自己的错误代码，而另一些人则选择在C++中使用类似异常的结构。

实现细节可能会非常复杂。例如，在遵循POSIX的系统中，由于线程本地存储（TLS）的魔法，`errno`是线程安全的。在嵌入式系统中，资源宝贵，可能会更倾向于使用自定义的错误处理代码，而不是那些可能增加软件膨胀的标准方法。

## 另请参阅

- 深入了解`errno`：https://en.cppreference.com/w/c/error/errno
- 关于线程安全和errno，请参阅POSIX线程：http://man7.org/linux/man-pages/man3/pthread_self.3.html
- 了解setjmp和longjmp的介绍：https://www.cplusplus.com/reference/csetjmp/
- C++中异常处理的资料：https://isocpp.org/wiki/faq/exceptions
