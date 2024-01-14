---
title:    "C#: 读取文本文件。"
keywords: ["C#"]
---

{{< edit_this_page >}}

为什么: 阅读文本文件是编程中非常重要的一步。它允许我们以可视化方式查看和处理数据，并可以将数据保存在我们的程序中。无论是分析日志文件还是读取用户输入，阅读文本文件都是必不可少的。

代码示例:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // 打开文本文件
        string path = @"C:\Users\Username\Desktop\sample.txt";
        StreamReader reader = new StreamReader(path);

        // 读取文本文件中的每一行，并打印在控制台上
        string line = "";
        while((line = reader.ReadLine()) !=null)
        {
            Console.WriteLine(line);
        }

        // 关闭文件
        reader.Close();
    }
}
```

示例输出: 

```
Hello, World!
This is a sample file.
It contains some data for us to read.
```

深入了解: 当我们打开一个文本文件时，它是如何被读取的呢？文本文件是由一系列字符构成的，每个字符都有其对应的ASCII码值。在C#中，我们使用StreamReader类来读取文本文件。该类具有一个ReadLine()方法，它会读取文件中的每一行，并将其返回为一个字符串。我们可以使用while循环来重复调用ReadLine()方法，直到文件末尾被达到。最后，我们需要调用Close()方法来关闭文件并释放系统资源。

看看: 

- [C# StreamReader文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.streamreader?view=netframework-4.8)
- [ASCII码表](https://www.ascii-code.com/)

## 参考链接：

[Markdown入门指南](https://www.markdownguide.org/)