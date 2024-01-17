---
title:                "创建临时文件"
html_title:           "C++: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 什么是临时文件？为什么要创建它们？
创建临时文件是指在计算机中创建一个临时文件来存储一些临时性的数据。程序员通常会创建临时文件来处理一些需要暂时存储的信息，比如从一个文件中读取数据并临时保存在一个文件中。

# 如何创建临时文件？
```C++
#include <iostream>
#include <cstdio>

int main() {

  // 使用tmpnam()函数来在系统默认的临时文件夹中创建一个临时文件名
  char filename[L_tmpnam];
  tmpnam(filename);

  // 使用tmpfile()函数来创建一个实际的临时文件
  FILE *tempFile = tmpfile();

  if (tempFile != NULL) {
    std::cout << "成功创建了临时文件！" << std::endl;
    std::cout << "临时文件名为：" << filename << std::endl;

    // 使用fprintf()函数向临时文件中写入一些数据
    fprintf(tempFile, "这是一个临时文件的例子\n");
    fprintf(tempFile, "这是一些临时数据\n");

    // 使用fseek()函数将文件指针定位在文件的起始位置
    fseek(tempFile, 0, SEEK_SET);

    // 使用fgets()函数从临时文件中读取数据并输出到屏幕上
    char buffer[50];
    while (fgets(buffer, 50, tempFile) != NULL) {
      std::cout << buffer;
    }
  } else {
    std::cout << "无法创建临时文件！" << std::endl;
  }

  // 关闭临时文件
  fclose(tempFile);

  return 0;
}
```

输出：
```
成功创建了临时文件！
临时文件名为：/tmp/tmp.abcdef
这是一个临时文件的例子
这是一些临时数据
```

# 深入了解
- 创建临时文件的概念最初出现在Unix操作系统中，用于为程序提供一种临时存储数据的方式。
- 除了使用标准库中的tmpnam()和tmpfile()函数，程序员也可以使用操作系统提供的临时文件功能来创建临时文件。
- 创建的临时文件一般会根据系统设置自动被删除，但是程序员也可以在程序中手动删除它们。

# 参考资料
- [C++ 参考手册： tmpnam() 函数](https://zh.cppreference.com/w/cpp/io/c/tmpnam)
- [C++ 参考手册： tmpfile() 函数](https://zh.cppreference.com/w/cpp/io/c/tmpfile)
- [CSDN 博客：临时文件在文件系统中的作用](https://blog.csdn.net/hongyang_xu/article/details/7847467)