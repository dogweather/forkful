---
title:                "检查目录是否存在"
html_title:           "C++: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么是？为什么要检查目录是否存在？
检查一个目录是否存在是指在编程过程中，程序员会使用一个函数来判断指定的目录是否存在。这是因为在开发软件时，我们可能需要在程序中访问特定的目录来读取或写入文件。如果目录不存在，程序可能会发生错误或无法正常运行。因此，检查目录是否存在是一个重要的程序员技巧。

## 如何进行检查目录是否存在？
以下是一个C++的代码示例，展示如何检查一个目录是否存在，并输出结果：
```C++ 
#include <iostream>
#include <filesystem>
namespace fs = std::filesystem;

int main() {
    fs::path directory = "C:/Users/User/Desktop";
    if (fs::exists(directory)) {
        std::cout << "目录 " << directory << " 存在！" << std::endl;
    } else {
        std::cout << "目录 " << directory << " 不存在！" << std::endl;
    }
    return 0;
}
```
输出结果：
```
目录 C:/Users/User/Desktop 存在！
```

## 深入了解
- 历史背景：检查目录是否存在的方法在早期的编程语言中并不常见，直到后来的C语言中出现了 `opendir()` 函数，提供了一种方便的方法来检查目录是否存在。
- 替代方法：除了使用C++标准库中的 `exists()` 函数，还可以使用操作系统提供的命令来检查目录是否存在。例如，在UNIX系统上，可以通过调用 `access()` 函数来完成检查。
- 实现细节：在C++代码中使用 `exists()` 函数时，其实质是通过 `stat()` 系统调用来检查给定路径指向的文件是否存在。

## 参考资料
- [C++ filesystem library](https://en.cppreference.com/w/cpp/filesystem)
- [检查目录是否存在的最佳实践](https://www.codeproject.com/Articles/17365/C-Program-to-check-if-a-Given-File-or-Directory-ex)
- [使用access()函数来检查目录是否存在](https://www.geeksforgeeks.org/c-program-check-given-file-exists-not/)