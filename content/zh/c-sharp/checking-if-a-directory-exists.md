---
title:                "C#: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

当我们在编程过程中需要经常读取或写入文件时，我们需要先确保所需的目录是否存在。这样我们才能对文件进行正确的操作。如果我们没有检查目录的存在性，可能会导致程序出错或无法正常运行。

## 如何操作

在C#中，我们可以使用Directory类的Exists方法来检查目录是否存在。首先，我们需要导入System.IO命名空间。接下来，在主函数中，我们可以使用if语句来判断目录是否存在，并打印出相应的结果。

```
using System;
using System.IO;

class Program
{
    static void Main()
    {
        if (Directory.Exists(@"C:\Users\Username\Desktop\NewFolder"))
        {
            Console.WriteLine("目录存在！");
        }
        else
        {
            Console.WriteLine("目录不存在！");
        }
    }
}
```

输出结果：

```
目录存在！
```

## 深入了解

除了使用Exists方法，我们还可以使用其他方法来检查目录的存在性。例如，可以通过调用Directory.GetDirectories方法来获取指定路径下的目录数组，并判断目录数组中是否包含了我们需要的目录名称。

## 参考链接

- Directory.Exists 方法 (System.IO) - https://docs.microsoft.com/zh-cn/dotnet/api/system.io.directory.exists?view=netframework-4.8
- About DirectoryInfo and FileInfo (System.IO) - https://docs.microsoft.com/zh-cn/dotnet/standard/io/file-directory-and-drive-manipulation
- Directory Class (System.IO) - https://docs.microsoft.com/zh-cn/dotnet/api/system.io.directory?view=netframework-4.8
- System.IO命名空间 - https://docs.microsoft.com/zh-cn/dotnet/api/system.io?view=netframework-4.8#properties
- Microsoft: C# Documentation - https://docs.microsoft.com/zh-cn/dotnet/csharp/