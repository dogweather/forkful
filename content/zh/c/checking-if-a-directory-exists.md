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

## What & Why? (什么与为什么?)

在C语言中确认文件夹是否存在是检测给定的文件夹路径能否在文件系统找到的过程。程序员做这个的原因是为了避免在不存在的文件夹上执行操作，从而造成错误。

## How to (如何操作):

```C
#include <stdio.h>
#include <sys/stat.h>
#include <stdbool.h>

bool doesDirectoryExist(const char *path) {
    struct stat statbuf;
    
    if (stat(path, &statbuf) != 0) {
        return false;
    }
    
    return S_ISDIR(statbuf.st_mode);
}

int main() {
    const char *path = "/path/to/directory";
    
    if (doesDirectoryExist(path)) {
        printf("Directory exists.\n");
    } else {
        printf("Directory does not exist.\n");
    }
    
    return 0;
}
```

Sample output (样例输出):

```
Directory exists.
```
or (或者)

```
Directory does not exist.
```

## Deep Dive (深入探索):

这个功能的实现利用了 `stat` 函数，这个函数尝试获取文件状态，并填充 `stat` 结构体。如果路径表示的是目录，`S_ISDIR` 宏检查 `st_mode` 字段确认它。

对于历史环境而言，此方法已经存在多年，是Unix和类Unix系统的标准部分。Windows系统有其他的API调用方式。实际上，有很多其他方法可以实现同样的目标，例如使用 `opendir()` 函数和C++17中的 `std::filesystem::exists()`。

实施细节方面，考虑文件的权限和可能的错误处理是很重要的。例如，如果程序运行在没有读取特定文件夹权限的用户下，`stat()` 调用可能失败。

## See Also (另请参阅):

- C标准库的 `stat` 文档：https://en.cppreference.com/w/c/io/stat
- POSIX `stat` 参考：https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html
- C++ `std::filesystem` 参考：https://en.cppreference.com/w/cpp/filesystem

请注意，网站链接可能是英文的。相关技术阅读时应考虑翻译或对应的中文资源。