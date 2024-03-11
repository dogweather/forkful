---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:20.572222-07:00
description: "\u5728 C++ \u4E2D\u5411\u6587\u672C\u6587\u4EF6\u5199\u5165\u6570\u636E\
  \u6D89\u53CA\u521B\u5EFA\u6216\u6253\u5F00\u6587\u4EF6\uFF0C\u7136\u540E\u5C06\u6570\
  \u636E\u5199\u5165\u5176\u4E2D\uFF0C\u8FD9\u662F\u9700\u8981\u6301\u4E45\u5316\u6570\
  \u636E\u7684\u5E94\u7528\u7A0B\u5E8F\uFF08\u5982\u65E5\u5FD7\u3001\u7528\u6237\u751F\
  \u6210\u7684\u5185\u5BB9\u6216\u914D\u7F6E\u8BBE\u7F6E\uFF09\u7684\u57FA\u672C\u4EFB\
  \u52A1\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4FDD\u5B58\u7A0B\
  \u5E8F\u6267\u884C\u671F\u95F4\u751F\u6210\u7684\u6570\u636E\uFF0C\u6216\u4E3A\u4E86\
  \u5C06\u6570\u636E\u5BFC\u51FA\u4EE5\u4F9B\u5176\u4ED6\u7A0B\u5E8F\u6216\u7528\u6237\
  \u4F7F\u7528\u3002"
lastmod: '2024-03-11T00:14:21.933186-06:00'
model: gpt-4-0125-preview
summary: "\u5728 C++ \u4E2D\u5411\u6587\u672C\u6587\u4EF6\u5199\u5165\u6570\u636E\u6D89\
  \u53CA\u521B\u5EFA\u6216\u6253\u5F00\u6587\u4EF6\uFF0C\u7136\u540E\u5C06\u6570\u636E\
  \u5199\u5165\u5176\u4E2D\uFF0C\u8FD9\u662F\u9700\u8981\u6301\u4E45\u5316\u6570\u636E\
  \u7684\u5E94\u7528\u7A0B\u5E8F\uFF08\u5982\u65E5\u5FD7\u3001\u7528\u6237\u751F\u6210\
  \u7684\u5185\u5BB9\u6216\u914D\u7F6E\u8BBE\u7F6E\uFF09\u7684\u57FA\u672C\u4EFB\u52A1\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4FDD\u5B58\u7A0B\u5E8F\
  \u6267\u884C\u671F\u95F4\u751F\u6210\u7684\u6570\u636E\uFF0C\u6216\u4E3A\u4E86\u5C06\
  \u6570\u636E\u5BFC\u51FA\u4EE5\u4F9B\u5176\u4ED6\u7A0B\u5E8F\u6216\u7528\u6237\u4F7F\
  \u7528\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 C++ 中向文本文件写入数据涉及创建或打开文件，然后将数据写入其中，这是需要持久化数据的应用程序（如日志、用户生成的内容或配置设置）的基本任务。程序员这样做是为了保存程序执行期间生成的数据，或为了将数据导出以供其他程序或用户使用。

## 如何操作：
C++ 提供了几种写入文本文件的方法，但其中一种最直接的方法是使用 `<fstream>` 库，它提供了为文件写入操作设计的 `ofstream`（输出文件流）类。

### 使用 `<fstream>` 的示例：

```cpp
#include <fstream>
#include <iostream>

int main() {
    std::ofstream file("example.txt");
    if (file.is_open()) {
        file << "Hello, world!\n";
        file << "Writing to a file in C++ is simple.";
        file.close();
    } else {
        std::cerr << "Failed to open file\n";
    }
    return 0;
}
```

**'example.txt' 中的示例输出：**
```
Hello, world!
Writing to a file in C++ is simple.
```

当处理更复杂的数据或需要更多控制写入过程时，程序员可能会转向 Boost Filesystem 等第三方库。

### 使用 Boost Filesystem 的示例：

要使用 Boost 进行文件操作，您首先需要安装 Boost 库。以下示例演示了使用 `boost::filesystem` 和 `boost::iostreams` 创建和写入文件。

```cpp
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>
#include <iostream>

namespace io = boost::iostreams;
namespace fs = boost::filesystem;

int main() {
    fs::path filePath("boost_example.txt");
    io::stream_buffer<io::file_sink> buf(filePath.string());
    std::ostream out(&buf);
    out << "Boost makes file operations easy.\n";
    out << "This is a line written with Boost.";
    
    return 0;
}
```

**'boost_example.txt' 中的示例输出：**
```
Boost makes file operations easy.
This is a line written with Boost.
```

在原生 C++ 和像 Boost 这样的第三方库之间的选择可能取决于您的项目的具体要求，以及您对文件 I/O 操作需要多少控制或灵活性。
