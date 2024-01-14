---
title:                "C++: 读取文本文件"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

有成千上万的编程语言可供选择，但在当今的软件行业中，有一门语言绝对不能被忽视：C++。无论是游戏开发、移动应用程序还是操作系统，C++都是一个强大的选择。今天，我们将探讨如何使用C ++来读取文本文件，并深入了解该过程的内部工作原理。

## 为什么要读取文本文件？

读取文本文件在编程中是一个非常常见的任务。文本文件可以包含各种信息，例如配置设置、日志文件、数据库导出等。通过读取这些文件，我们可以从中提取有用的信息并在程序中使用。这是一个非常实用和重要的技能，可以帮助我们轻松处理各种任务。

## 如何做到？

要使用C++读取文本文件，我们首先需要学习一些基本知识。在C++中，我们可以使用标准库中的iostream头文件来处理输入和输出流。让我们看一个简单的例子，假设我们有一个文本文件"example.txt"，其中包含以下内容：

```C++
Hello World!
This is an example text file.
```

要读取这个文件，我们可以使用C++的ifstream类来创建一个输入流对象，并将文件作为参数传递给它。然后，我们可以使用输入操作符“>>”来逐行读取文件内容，并将它们存储在一个字符串变量中。

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    // 创建输入流对象
    ifstream inputFile("example.txt");
    // 创建一个字符串变量来存储读取的内容
    string line;

    // 使用输入操作符“>>”来逐行读取文件内容，并将其打印到控制台
    while (inputFile >> line) {
        cout << line << endl;
    }

    // 关闭输入流
    inputFile.close();

    return 0;
}
```

上面的代码将输出：

```
Hello
World!
This
is
an
example
text
file.
```

通过这种方式，我们可以轻松地读取文本文件中的内容，并在程序中使用。

## 深入探讨

那么，当我们使用输入流读取文本文件时，它究竟是如何工作的呢？其实，它的工作原理并不复杂。当我们创建输入流对象后，C++会检查文件是否存在，如果存在，它会将文件内容转换为输入流对象的缓冲区。然后，每次我们使用输入操作符“>>”时，它会从缓冲区中读取一个字符，并将其存储在相关的变量中，直到文件末尾。

值得注意的是，C++并不会跟踪我们读取的文件行数。如果我们想要按行读取文件中的内容，可以使用getline函数来实现，这也是更好的选择。

## 参考链接

- [C ++文本文件的读取和写入](https://www.geeksforgeeks.org/readwrite-file-c/)
- [C ++标准库文档](https://en.cppreference.com/w/)

## 参见

- [如何用C++写入文本文件？](https://github.com/jlebon/casual-cpp/blob/master/files/write-file.md)
- [如何在C++中读写二进制文件？](https://github.com/jlebon/casual-cpp/blob/master/files/binary-files.md)