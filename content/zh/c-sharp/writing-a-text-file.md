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

## 为什么

当我们编写程序时，有时候会需要将数据保存在一个文本文件中。这样可以方便我们将数据读取到其他程序中，或者作为备份保留。因此，学会如何编写文本文件在编程中是非常重要的。

## 如何

首先，我们需要使用C#中的"File"类来创建一个文本文件。我们可以使用"StreamWriter"类来将文本写入文件中。下面是一个简单的例子，展示了如何创建并写入文本文件。

```C#
using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        // 创建并打开文本文件
        using (StreamWriter writer = new StreamWriter("myfile.txt"))
        {
            // 写入文本内容
            writer.WriteLine("这是我的第一个文本文件");
            writer.WriteLine("Hello World!");
        }
    }
}
```

上面的代码使用"using"语句创建并打开了一个名为"myfile.txt"的文本文件，并使用"StreamWriter"类将两行文本写入文件中。我们可以在代码执行后的相同目录下找到这个文本文件。下面是它的内容：

```
这是我的第一个文本文件
Hello World!
```

## 深入了解

除了简单的文本数据，我们也可以向文本文件写入更复杂的内容，例如整数、浮点数和布尔值等。我们可以使用"Write"和"WriteLine"方法来分别写入不同类型的数据。另外，我们还可以使用"Append"方法在已存在的文本文件中追加内容。

另一个需要注意的点是，为了防止在写入文件时出现中文乱码，我们可以在创建"StreamWriter"实例时指定文本编码方式，例如：

```C#
StreamWriter writer = new StreamWriter("myfile.txt", false, Encoding.UTF8);
```

如果我们想要读取文本文件中的数据，我们可以使用"StreamReader"类来打开并读取文件内容。类似地，在读取文件时也可以指定文本编码方式。

## 参考链接

- [C#文本文件创建和写入](https://www.runoob.com/csharp/csharp-text-files.html)
- [C#文件操作指南](https://www.cnblogs.com/zhaoqingqing/p/4521002.html)
- [C# StreamWriter类的使用](https://www.cnblogs.com/linlf03/p/5203805.html)

# 参见

- [C#学习之旅](https://www.runoob.com/csharp/csharp-tutorial.html)
- [C#编程常见问题解答](https://www.runoob.com/csharp/programming-faq.html)
- [C#官方文档](https://docs.microsoft.com/zh-cn/dotnet/csharp/)