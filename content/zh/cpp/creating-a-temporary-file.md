---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么是临时文件，为什么要创建？
临时文件是在运行过程中由程序创建并使用的文件，执行完毕便被删除。程序员创建临时文件主要用于存储大量数据，以节省内存，或用于不同程序间的通信。

## 怎么做：
以下是C++实例演示如何创建一个临时文件。

```C++
#include <cstdio>

int main() {
    char tmpname[L_tmpnam];
    char* filename = std::tmpnam(tmpname);
    
    std::cout << "Temp file name is: " << filename << std::endl;

    std::fstream fs(filename);
    fs << "Test text." << std::endl;
    fs.close();

    return 0;
}
```

上述代码执行后将显示系统为临时文件分配的文件名，并在该文件中写入字符串 "Test text."。

## 深入阐述
在过去的C++版本中，创建临时文件一般使用 `tmpnam()`函数。然而，由于可能的竞态条件，这个函数已不再推荐使用。替代方案是使用 `tmpfile()`函数，这无需手动删除临时文件。另一个替代方案是使用高级库如Boost.Filesystem，它提供了 `unique_path()`函数用以创建临时文件。

当创建临时文件时，需要注意文件间可能出现的竞态条件。竞态条件是指在多线程系统中，任务的执行序列依赖于任务间的相对速度。

## 参考资料：
1. https://en.cppreference.com/w/cpp/io/c/tmpnam
2. https://www.boost.org/doc/libs/1_63_0/libs/filesystem/doc/reference.html#unique_path
3. https://en.cppreference.com/w/cpp/utility/program/tmpfile
4. https://www.cplusplus.com/reference/cstdio/tmpnam/