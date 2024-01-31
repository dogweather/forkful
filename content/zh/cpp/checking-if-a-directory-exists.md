---
title:                "检查目录是否存在"
date:                  2024-01-19
simple_title:         "检查目录是否存在"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)
检查目录是否存在是判断给定路径的文件夹是否在文件系统中存在的过程。程序员这么做是为了避免在目录不存在时进行无意义的文件操作，造成错误。

## How to: (如何做：)
使用 `<filesystem>` 库检查目录。下面是示例代码：

```C++
#include <iostream>
#include <filesystem>

int main() {
    std::string dir = "/path/to/directory";

    if (std::filesystem::exists(dir)) {
        std::cout << "Directory exists!" << std::endl;
    } else {
        std::cout << "Directory does not exist." << std::endl;
    }

    return 0;
}
```

如果目录存在，输出将是：

```
Directory exists!
```

如果目录不存在，输出将是：

```
Directory does not exist.
```

## Deep Dive (深入了解)
在 `<filesystem>` 库之前，C++ 没有标准化方法检查目录，需要依赖平台特定代码或第三方库。`<filesystem>` 是 C++17 引入的，提供了跨平台处理文件系统的方式。此外，还可以使用 `std::filesystem::is_directory` 来特定检查路径是否是一个目录，这确保了路径不仅存在而且是正确的类型。`<filesystem>` 的引入让文件和目录操作程简化了许多。

## See Also (另请参阅)
- C++17 `<filesystem>` 文档: [cppreference.com](https://en.cppreference.com/w/cpp/filesystem)
- Boost 文件系统库(在 `<filesystem>` 出现之前的选择): [boost.org](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)
- C++ 文件IO 教程: [cplusplus.com](http://www.cplusplus.com/doc/tutorial/files/)
