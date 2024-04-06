---
date: 2024-01-20 17:53:56.781884-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1F) \u6587\u672C\u6587\u4EF6\u8BFB\u53D6\
  \u4E0D\u590D\u6742\uFF0C\u4F46\u6709\u6DF1\u5EA6\u3002\u65E9\u5728C\u8BED\u8A00\u6807\
  \u51C6\u5E93\u4E2D\u5C31\u6709\u4E86`fopen`\u3001`fgets`\u7B49\u51FD\u6570\u3002\
  C++\u5F15\u5165\u4E86\u6587\u4EF6\u6D41\uFF08fstream\uFF09\uFF0C\u63D0\u4F9B\u4E86\
  \u66F4\u76F4\u89C2\u7684\u64CD\u4F5C\u65B9\u5F0F\u3002\u4E0D\u6B62`ifstream`\uFF0C\
  `stringstream`\u53EF\u4EE5\u8BFB\u53D6\u5B57\u7B26\u4E32\u6D41\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.339057-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A\uFF1F) \u6587\u672C\u6587\u4EF6\u8BFB\u53D6\u4E0D\u590D\
  \u6742\uFF0C\u4F46\u6709\u6DF1\u5EA6\u3002\u65E9\u5728C\u8BED\u8A00\u6807\u51C6\u5E93\
  \u4E2D\u5C31\u6709\u4E86`fopen`\u3001`fgets`\u7B49\u51FD\u6570\u3002C++\u5F15\u5165\
  \u4E86\u6587\u4EF6\u6D41\uFF08fstream\uFF09\uFF0C\u63D0\u4F9B\u4E86\u66F4\u76F4\u89C2\
  \u7684\u64CD\u4F5C\u65B9\u5F0F\u3002\u4E0D\u6B62`ifstream`\uFF0C`stringstream`\u53EF\
  \u4EE5\u8BFB\u53D6\u5B57\u7B26\u4E32\u6D41\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

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
