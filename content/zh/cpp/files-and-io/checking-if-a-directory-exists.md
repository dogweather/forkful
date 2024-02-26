---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:52.254565-07:00
description: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u662F\u6307\u5728\u6267\
  \u884C\u5982\u4ECE\u4E2D\u8BFB\u53D6\u6216\u5411\u5176\u5199\u5165\u6587\u4EF6\u7B49\
  \u64CD\u4F5C\u524D\uFF0C\u786E\u5B9A\u6307\u5B9A\u8DEF\u5F84\u4E0A\u76EE\u5F55\u7684\
  \u5B58\u5728\u6027\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u907F\
  \u514D\u4E0E\u6587\u4EF6\u64CD\u4F5C\u76F8\u5173\u7684\u9519\u8BEF\uFF0C\u786E\u4FDD\
  \u4ED6\u4EEC\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u7684\u6587\u4EF6\u5904\u7406\u4EFB\
  \u52A1\u80FD\u66F4\u987A\u7545\u3001\u66F4\u53EF\u9760\u5730\u6267\u884C\u3002"
lastmod: '2024-02-25T18:49:45.693867-07:00'
model: gpt-4-0125-preview
summary: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u662F\u6307\u5728\u6267\
  \u884C\u5982\u4ECE\u4E2D\u8BFB\u53D6\u6216\u5411\u5176\u5199\u5165\u6587\u4EF6\u7B49\
  \u64CD\u4F5C\u524D\uFF0C\u786E\u5B9A\u6307\u5B9A\u8DEF\u5F84\u4E0A\u76EE\u5F55\u7684\
  \u5B58\u5728\u6027\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u907F\
  \u514D\u4E0E\u6587\u4EF6\u64CD\u4F5C\u76F8\u5173\u7684\u9519\u8BEF\uFF0C\u786E\u4FDD\
  \u4ED6\u4EEC\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u7684\u6587\u4EF6\u5904\u7406\u4EFB\
  \u52A1\u80FD\u66F4\u987A\u7545\u3001\u66F4\u53EF\u9760\u5730\u6267\u884C\u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
---

{{< edit_this_page >}}

## 什么与为什么？
检查目录是否存在是指在执行如从中读取或向其写入文件等操作前，确定指定路径上目录的存在性。程序员这样做是为了避免与文件操作相关的错误，确保他们的应用程序中的文件处理任务能更顺畅、更可靠地执行。

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
