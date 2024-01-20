---
title:                "读取文本文件"
html_title:           "Kotlin: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么与为什么？

读取文本文件具体指的是把文件中的字符数据载入到内存中去。程序员读取文本文件主要是为了处理或操作这些数据。

## 如何做？

在C#中，`System.IO`命名空间提供了处理文件的各种方法。例如，你可以使用`File`类的 `ReadAllLines` 方法来读取文本文件。以下就是一个简约的代码：

```C#
using System.IO;

foreach (string line in File.ReadLines(filepath))
{
    Console.WriteLine(line);
}
```

在这个例子中，将会读取指定文件路径的所有行，并打印到控制台。

## 深入了解：

关于读取文本文件，一些更深入的信息包括：

**历史背景：** 在早期的计算机语言中，例如COBOL和FORTRAN，文件读取是一个非常复杂的过程。直到现代的面向对象的编程语言如C#的出现，才使文件操作变得更简单和高效。

**替代方案：** `StreamReader` 是一个常见的替代方案。它使用 `ReadToEnd` 方法可以一次性读取整个文本，但要注意这在处理大文件时可能会导致内存不足：

```C#
using System.IO;

using (StreamReader sr = new StreamReader(filepath))
{
    Console.WriteLine(sr.ReadToEnd());
}
```

**实现细节：** `File.ReadLines` 和 `File.ReadAllLines` 之间的主要区别在于前者是面向行的迭代器，而后者是纯粹的读取操作。这意味着 `ReadLines` 在大文件上的表现更优，因为它不需要把整个文件放入内存。

## 参考链接：

1. [Microsoft 官方文档 - File 类](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.file)
2. [Microsoft 官方文档 - StreamReader 类](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.streamreader)
3. [StackOverflow - 有关读取文件的讨论](https://stackoverflow.com/questions/6437162/reading-large-text-files-with-streams-in-c-sharp)