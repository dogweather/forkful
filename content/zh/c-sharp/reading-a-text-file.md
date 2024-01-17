---
title:                "读取文本文件"
html_title:           "C#: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

何时何地读取文本文件？

读取文本文件是指从计算机中读取存储的文本信息，这些信息通常被保存在文本文件中。作为程序员，我们在编写程序时经常需要读取文本文件，因为它们包含了我们需要处理的大量数据。

如何读取文本文件：

```c#
// 使用StreamReader类打开文本文件
var reader = new StreamReader("file.txt");

// 使用ReadToEnd方法读取文件中的所有文本
string text = reader.ReadToEnd();

// 关闭文件流
reader.Close();

// 输出文件中的文本
Console.WriteLine(text);
```

你也可以使用其他类似的方法来读取文本文件，但StreamReader是最常见的选择。

深入探讨：

读取文本文件在计算机编程中已经存在很长一段时间了。在过去，我们可能使用DOS命令来读取文本文件，但现在有了更多的选择。除了StreamReader类，我们还可以使用File类、FileStream类等来读取文本文件。这些类都有自己独特的优势和适用场景，但原理都是类似的。

相关链接：

- [Microsoft官方文档：StreamReader类](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.streamreader?view=net-5.0)
- [C# 程序员的文本文件读取指南](https://www.c-sharpcorner.com/article/text-file-processing-in-C-Sharp/)