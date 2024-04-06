---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:52.254565-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5728\u73B0\u4EE3C++\uFF08C++17\u53CA\u4EE5\
  \u540E\u7248\u672C\uFF09\u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u6587\u4EF6\u7CFB\
  \u7EDF\u5E93\u6765\u68C0\u67E5\u4E00\u4E2A\u76EE\u5F55\u662F\u5426\u5B58\u5728\u3002\
  \u5B83\u63D0\u4F9B\u4E86\u4E00\u79CD\u76F4\u63A5\u4E14\u6807\u51C6\u5316\u7684\u65B9\
  \u5F0F\u6765\u6267\u884C\u6587\u4EF6\u7CFB\u7EDF\u64CD\u4F5C\uFF0C\u5305\u62EC\u68C0\
  \u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u3002"
lastmod: '2024-04-05T21:53:48.419525-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

## 如何做：
在现代C++（C++17及以后版本）中，你可以使用文件系统库来检查一个目录是否存在。它提供了一种直接且标准化的方式来执行文件系统操作，包括检查目录是否存在。

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "该目录存在。" << std::endl;
    } else {
        std::cout << "该目录不存在。" << std::endl;
    }

    return 0;
}
```
如果目录存在，示例输出为：
```
该目录存在。
```

如果目录不存在，示例输出为：
```
该目录不存在。
```

对于尚未使用C++17或需要额外功能的项目，Boost文件系统库是一种流行的第三方选择，提供了类似的功能。

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "该目录存在。" << std::endl;
    } else {
        std::cout << "该目录不存在。" << std::endl;
    }

    return 0;
}
```
使用Boost文件系统，输出将与C++17文件系统示例相同，具体取决于指定路径上目录的存在性。
