---
title:                "阅读文本文件"
date:                  2024-01-20T17:53:56.781884-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

读取文本文件就是获取文件内容到程序里。程序员这样做主要是为了处理和分析数据。

## How to: (怎么做？)

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("example.txt"); // 打开文件
    std::string line;

    if (file.is_open()) {
        while (getline(file, line)) { // 循环读取每一行
            std::cout << line << '\n'; // 打印到控制台
        }
        file.close(); // 关闭文件
    } else {
        std::cout << "Unable to open file";
    }

    return 0;
}
```

**Sample Output:**

```
Hello, World!
This is an example text file.
Read me, please!
```

## Deep Dive (深入了解)

文本文件读取不复杂，但有深度。早在C语言标准库中就有了`fopen`、`fgets`等函数。C++引入了文件流（fstream），提供了更直观的操作方式。不止`ifstream`，`stringstream`可以读取字符串流。

替代方案多：C++17带来的`filesystem`库，或者`mmap`用于内存映射文件。选择取决于需要：性能敏感用`mmap`，简单场景用文件流。

重要细节：编码问题和异常处理。用`wifstream`读取宽字符文本，注意设置正确的locale。打开文件前检查是否存在，用`try-catch`处理异常。

## See Also (另请参阅)

- C++ File I/O documentation: [http://www.cplusplus.com/reference/fstream/](http://www.cplusplus.com/reference/fstream/)
- C++17 `filesystem` library: [https://en.cppreference.com/w/cpp/filesystem](https://en.cppreference.com/w/cpp/filesystem)