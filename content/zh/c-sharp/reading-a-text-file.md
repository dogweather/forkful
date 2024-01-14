---
title:                "C#: 读取文本文件"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

阅读和处理文本文件可能是每位程序员必须要学习和掌握的基础技能。文本文件是计算机中最常见的数据存储形式，通过读取文本文件，我们可以获取和处理各种信息，从而使我们的程序更加灵活和功能强大。

## 如何做

要在C#中读取文本文件，我们可以使用 `StreamReader` 类。以下是一个简单的例子，展示如何使用该类来读取文本文件并打印出其中的内容：

```C#
using System;
using System.IO;

namespace ReadingTextFile
{
    class Program
    {
        static void Main(string[] args)
        {
            // 读取文本文件的路径
            string filePath = "C:/Users/username/Desktop/textfile.txt";

            // 实例化一个StreamReader对象来读取文本文件
            StreamReader reader = new StreamReader(filePath);

            // 创建一个文本字符串，用于存储文本文件的内容
            string text = "";

            // 使用StreamReader对象的ReadToEnd()方法将文本文件的内容读取到text变量中
            text = reader.ReadToEnd();

            // 关闭StreamReader对象
            reader.Close();

            // 在控制台打印文本文件的内容
            Console.WriteLine(text);
        }
    }
}
```

运行上面的代码，如果文本文件存在并且路径正确，你会在控制台看到文件中的内容被打印出来。

## 深入了解

除了上面介绍的基础用法外，我们还可以通过 `StreamReader` 类的其他方法来更灵活地处理文本文件。比如，我们可以使用 `ReadLine()` 方法来逐行读取文本文件的内容，或者使用 `Read()` 方法来读取文本文件中的特定字符。此外，我们还可以通过指定编码方式来读取不同格式的文本文件。

作为程序员，我们需要了解不同的方法和技巧，以便更有效地处理和读取文本文件。

## 参考链接

1. [C#文本文件教程](https://www.runoob.com/csharp/csharp-files.html)
2. [StreamReader类官方文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.streamreader?view=net-5.0)
3. [C# StreamReader类教程](https://www.geeksforgeeks.org/file-readalllines-method-in-c-sharp-with-examples/)