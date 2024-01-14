---
title:                "C: 检查目录是否存在"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

在C语言编程中，我们经常需要检查某个目录是否存在。这可以帮助我们避免在程序中使用不存在的目录，从而导致运行错误。

## 如何

检查目录是否存在可以使用C语言中的`opendir()`函数。下面是一个例子：

```
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>

int main()
{
    char *dir_path = "./my_directory"; // 需要检查的目录路径
    DIR *dir = opendir(dir_path); // 打开目录
    if (dir) // 如果成功打开目录，则目录存在
    {
        printf("Directory exists.\n");
        closedir(dir); // 关闭目录
    }
    else // 否则，目录不存在
    {
        printf("Directory does not exist.\n");
        exit(1);
    }
    return 0;
}
```

运行以上代码，如果`my_directory`目录存在，则会输出`Directory exists.`，如果目录不存在，则会输出`Directory does not exist.`。

## 深入了解

`opendir()`函数使用`DIR`数据类型来表示一个打开的目录。它的返回值为`NULL`表示打开失败，也就是目录不存在。我们也可以使用`closedir()`函数来关闭已经打开的目录。

此外，我们也可以使用`access()`函数来检查文件或目录是否存在。它的第一个参数为文件或目录的路径，第二个参数为检查的权限，这里我们使用`F_OK`表示检查是否存在。如果返回值为0，则表示文件或目录存在，否则不存在。

## 参考资料

- `opendir()`函数文档: https://www.cplusplus.com/reference/cstdio/opendir/
- `access()`函数文档: https://www.cplusplus.com/reference/cstdio/access/

## 参见

- `readdir()`函数文档: https://www.cplusplus.com/reference/cstdio/readdir/ (用于读取目录内的文件)