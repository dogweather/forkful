---
title:                "编写文本文件"
html_title:           "C++: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 什么是文本文件? 为什么程序员要写文本文件?

文本文件是一种纯文本格式的文件，可以包含文本、数字和符号。程序员通常会使用文本文件来存储数据或保存程序的设置信息。这种文件具有可读性，使其易于编辑和共享，因此它们是编程中很常见的一种形式。

# 如何:

```c++
// 创建并打开一个文本文件
#include <fstream> // 包含文件系统库

int main() {
  // 创建一个名为 "data.txt" 的文件并打开它
  // "ofstream" 表示我们要写入文件
  std::ofstream file("data.txt");

  // 将文本写入文件
  file << "Hello World!"; // 在现有文件末尾写入 "Hello World!" 字符串
  file << 2021; // 再次写入文本，这次是数字

  // 关闭文件
  file.close();

  return 0;
}
```

文件"**data.txt**"将包含以下内容：
```
Hello World!
2021
```

# 深入了解:

在计算机发展的早期阶段，文本文件是主要的数据存储格式。它们只包含文本，在未来会受到更复杂的二进制格式的取代。现在，文本文件仍然非常普遍，而且可以轻松地使用编程语言来读写它们。

除了文本文件外，还有许多其他存储数据的格式，例如数据库和二进制文件。这些格式具有不同的优缺点，具体取决于具体的使用场景。

在C++中，我们可以使用标准库函数来读写文本文件。使用`ofstream`可以创建一个文件来写入文本，使用`ifstream`则可以打开一个已有文件来读取文本。

# 查看也:

- 关于文件系统库的更多信息可以在这里找到: https://en.cppreference.com/w/cpp/filesystem
- 想要了解更多有关文本文件的背景知识，可以阅读这篇文章：https://en.wikipedia.org/wiki/Text_file
- 为什么要使用数据库而不是文本文件？您可以在这里找到答案：https://stackify.com/a-beginners-guide-to-database-file-format/