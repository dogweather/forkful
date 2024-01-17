---
title:                "编写文本文件"
html_title:           "C#: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么是文本文件？为什么程序员需要写文本文件？
写文本文件是一种将文本内容保存在计算机上的方式。程序员经常需要写文本文件来保存数据或配置信息，以及在程序执行时读取这些文件的内容。

## 如何写文本文件：
\`\`\`C#
// 创建文本文件
File.WriteAllText(@"C:\Users\John\Desktop\sample.txt", "Hello world!");
// 读取文本文件
string content = File.ReadAllText(@"C:\Users\John\Desktop\sample.txt");
Console.WriteLine(content);
// 输出：Hello world!
\`\`\`

## 深入了解：
写文本文件的历史可以追溯到计算机的发展早期，最初是用来存储和传输数据。现在，我们有许多不同的文件格式来存储更复杂的数据，但文本文件仍然是最简单、最普遍的数据存储格式。除了使用C#的File类来写文本文件，还有其他的编程语言和库也可以实现相同的功能。

## 参考链接：
- [C# File类文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.file?view=netcore-3.1)
- [文本文件的定义和作用](https://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E6%96%87%E4%BB%B6)