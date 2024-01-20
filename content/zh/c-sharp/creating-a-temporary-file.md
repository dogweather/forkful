---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

创建临时文件是在你的程序运行期间动态生成一些可以读写的文件，但在程序完成时会自动删除的文件。程序员之所以这样做，是因为这可以帮助我们存储临时数据，减少内存压力，同时还能方便调试和数据交换。

## 如何操作:

在C#中，我们可以通过 `Path.GetTempFileName()` 方法创建临时文件。这个方法会自动为你创建一个带有 `.tmp` 扩展名的文件，并返回文件路径。

下面是一个示例：

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string tempFilePath = Path.GetTempFileName();
        Console.WriteLine(tempFilePath);
    }
}
```

运行以上代码，你将看到这样的输出：

```C#
C:\Users\Username\AppData\Local\Temp\tmp1234.tmp
```

这个输出显示我们创建的临时文件的访问路径。

## 深入探究：

在早期，临时文件主要用于存储大要处理大量要处理的数据，以减轻内存压力。现在它的使用更加多样化，不仅包括断点恢复，还可以进行跨程序数据共享等。

此外， .NET Framework 还提供了 `FileStream` 和 `StreamWriter` 方法以更加细粒度的方式来工作临时文件。

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string tempFile = Path.GetTempFileName();

        using (FileStream fs = File.OpenWrite(tempFile))
        using (StreamWriter sw = new StreamWriter(fs))
        {
            sw.WriteLine("Hello World");
        }

        using (FileStream fs = File.OpenRead(tempFile))
        using (StreamReader sr = new StreamReader(fs))
        {
            string line = sr.ReadLine();
            Console.WriteLine(line);
        }
        
        File.Delete(tempFile);
    }
}
```

当然，你也可以使用其他编程语言来创建临时文件，不过具体做法和C#有所不同。

## 查看更多：

如果想了解更多关于C#中文件操作的信息， 我推荐以下链接：

- [C# Directory 类](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.directory?view=net-5.0)
- [C# FileStream 类](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.filestream?view=net-5.0)
- [C# StreamWriter 类](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.streamwriter?view=net-5.0)
- [C# File 클래스](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.file?view=net-5.0)