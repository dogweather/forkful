---
title:                "检查目录是否存在"
html_title:           "C++: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

```Chinese (Simplified)```

## 什么与为何? ("What & Why?")

检查目录是否存在是在程序中确认特定文件夹是否已存在的过程。程序员之所以进行此操作，是因为才能更好地进行文件操作，例如文件存储或读取等。

## 如何操作: ("How to:")

我们可以使用 C++17 的 `<filesystem>` 库中的 `exists` 函数来检查目录是否存在。以下是一个简单的示例：

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    fs::path p{"/path/to/directory"};
    if (fs::exists(p)) {
        std::cout << "The directory exists.\n";
    } else {
        std::cout << "The directory does not exist.\n";
    }
    return 0;
}
```

运行此程序将会输出 `"The directory exists."`（如果目录存在） 或者 `"The directory does not exist."` （如果目录不存在）。

## 深入了解 ("Deep Dive")

在之前的C++版本中，并没有内置函数可以直接检查目录是否存在。通常的做法是尝试打开该目录或在其内创建一个文件。

C++17 通过引入 `<filesystem>` 库，使文件和目录操作变得更简单。此外，除了 `exists` 函数，我们还可以使用 `is_directory` 函数来确认一个路径是否指向一个目录。

`<filesystem>` 库的提供，让使用C++进行文件系统操作更加统一，减少了平台差异带来的麻烦。

## 更多参考 ("See Also")

更多信息和用例，可以参考以下链接：

- [`<filesystem>`](https://en.cppreference.com/w/cpp/filesystem)
- [`fs::exists`](https://en.cppreference.com/w/cpp/filesystem/exists)
- [`fs::is_directory`](https://en.cppreference.com/w/cpp/filesystem/is_directory)