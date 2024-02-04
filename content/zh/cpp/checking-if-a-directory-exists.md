---
title:                "检查目录是否存在"
date:                  2024-02-03T19:06:52.254565-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
