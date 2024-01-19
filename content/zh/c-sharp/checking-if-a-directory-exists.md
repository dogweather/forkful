---
title:                "检查目录是否存在"
html_title:           "C#: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么和为什么？

查看目录是否存在是检查特定文件夹路径是否真实存在的过程。程序员这么做是为了避免在访问不存在的目录时引发运行时异常。

## 如何做：

使用C#来检查目录是否存在相当简单。`System.IO.Directory`类中的`Exists`方法就可以实现这个功能。

```C#
using System.IO;

string dirToCheck = @"C:\Program Files";

if (Directory.Exists(dirToCheck)) 
{
    Console.WriteLine("The directory exists.");
} 
else 
{
    Console.WriteLine("The directory does not exist.");
}
```

运行这段代码，如果"C:\Program Files"目录存在，就会打印出"The directory exists."，否则会打印出"The directory does not exist."。

## 深度解析

检查目录存在性的方法在早期编程中并不常见，直到微软推出了.NET框架才引入这个方便的功能。

虽然C#中直接使用Directory.Exists是最直观的方法，但有一些其他的替代方案也可以实现同样的效果。例如，可以试图打开目录，如果抛出异常，则说明目录不存在。这是一种叫做"备选方案（EAFP）"的编程风格。

对于Directory.Exists的底层实现，.NET基础库在调用Directory.Exists时会直接调用一次系统API检查目录是否存在。而无论目录是否存在，这个系统API调用都会产生一定的性能开销。因此，如果你的程序需要在短时间内检查大量的目录，可能需要考虑其他的解决方案。

## 更多参阅

- Official Documentation for DirectoryInfo.Exists: https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=netframework-4.8
- File and Directory Operations in .NET: https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-check-that-a-file-or-directory-exists