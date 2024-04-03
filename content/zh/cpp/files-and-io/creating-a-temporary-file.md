---
date: 2024-01-20 17:39:39.209203-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u521B\u5EFA\u4E34\u65F6\u6587\
  \u4EF6\u53EF\u4EE5\u4F7F\u7528C++\u7684 `<filesystem>` \u5E93\u3002\u4E0B\u9762\u662F\
  \u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\u5B50\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.131813-06:00'
model: gpt-4-1106-preview
summary: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u53EF\u4EE5\u4F7F\u7528C++\u7684 `<filesystem>`\
  \ \u5E93\u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\u5B50\uFF1A\
  ."
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
weight: 21
---

## How to: (如何操作：)
创建临时文件可以使用C++的 `<filesystem>` 库。下面是一个简单的例子：

```C++
#include <iostream>
#include <filesystem>
#include <fstream>

int main() {
    std::filesystem::path temp_dir = std::filesystem::temp_directory_path();
    std::filesystem::path temp_file = temp_dir / "my_temp_file.txt";
    
    // 创建并使用临时文件
    std::ofstream out(temp_file.string());
    if (out.is_open()) {
        out << "这是一些临时的内容。\n";
        out.close();
        std::cout << "临时文件已创建：" << temp_file << '\n';
    } else {
        std::cerr << "无法创建临时文件。\n";
    }

    // 假设我们现在使用临时文件完成了工作……

    // 删除临时文件
    std::filesystem::remove(temp_file);
    std::cout << "临时文件已删除。\n";

    return 0;
}
```

输出可能类似：
```
临时文件已创建：/tmp/my_temp_file.txt
临时文件已删除。
```

## Deep Dive (深入探索)
在过去，创建临时文件常常依赖于操作系统特定的API或者使用诸如`tmpfile()`这样的C语言标准库函数。不过，随着C++17引入`<filesystem>`库，跨平台创建临时文件变得更加简单。

除了直接创建文件，`std::filesystem`也提供了`temp_directory_path`函数来找到合适的临时文件夹。用传统方式，譬如在UNIX系统中经常遇到环境变量`TMPDIR`未设置时需要手动指定临时路径。

为避免临时文件在使用后仍然占用空间，记得在不再需要时删除它们。这是通过`std::filesystem::remove`函数来实现。

## See Also (另请参阅)
- C++ Filesystem Library Documentation: https://en.cppreference.com/w/cpp/filesystem
- C++ Input/Output Library (std::fstream): https://en.cppreference.com/w/cpp/io/basic_fstream

追求更深入了解的读者，可以访问上述资料来探索C++文件系统以及输入输出库的其他高级特性。
