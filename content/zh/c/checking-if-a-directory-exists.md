---
title:    "C: 检查目录是否存在"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

在编写C语言程序时，有时候需要检查一个目录是否存在。这是因为在处理文件或者路径时，我们需要确定目录是否有效，以避免出现错误。

## 如何做

要检查一个目录是否存在，可以使用`opendir()`函数来打开目录，如果返回的指针为NULL，那么该目录就不存在。接下来，可以使用`closedir()`函数关闭目录指针。下面是一个简单的示例代码：

```C
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>

int main() {
    DIR *dirptr;

    // 打开目录
    dirptr = opendir("path/to/directory");

    // 检查目录是否存在
    if(dirptr == NULL) {
        printf("该目录不存在\n");
    } else {
        printf("该目录存在\n");

        // 关闭目录指针
        closedir(dirptr);
    }

    return 0;
}
```

当目录存在时，输出结果为`该目录存在`，当目录不存在时，输出结果为`该目录不存在`。

## 深入探讨

在上面的示例中，我们使用了`opendir()`函数来打开目录，该函数属于C标准库中的`dirent.h`头文件。在打开目录时，我们需要注意的是，传入的参数应该是一个有效的路径，否则函数将返回NULL。

如果需要对目录中的内容进行操作，需要使用`readdir()`函数来读取目录中的文件和子目录。除此之外，还可以使用`stat()`函数来获取目录的相关信息，如文件大小、创建时间等。

## 参考链接

- [C标准库手册 - opendir函数](https://zh.cppreference.com/w/c/io/opendir)
- [C标准库手册 - readdir函数](https://zh.cppreference.com/w/c/io/readdir)
- [C标准库手册 - stat函数](https://zh.cppreference.com/w/c/io/stat)
- [C语言目录操作详解](https://www.cnblogs.com/Anker/p/3271773.html)

## 参见

- [C语言文件操作：检查文件是否存在](https://learnku.com/articles/7184/checking-whether-a-file-exists-in-c-language)