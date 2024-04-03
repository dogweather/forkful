---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:04.975161-07:00
description: "\u5728 C# \u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u6D89\
  \u53CA\u9A8C\u8BC1\u6587\u4EF6\u7CFB\u7EDF\u4E2D\u6307\u5B9A\u8DEF\u5F84\u5904\u7684\
  \u6587\u4EF6\u5939\u662F\u5426\u5B58\u5728\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\
  \u9879\u68C0\u67E5\u662F\u4E3A\u4E86\u907F\u514D\u9519\u8BEF\uFF0C\u4F8B\u5982\u5C1D\
  \u8BD5\u4ECE\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u4E2D\u8BFB\u53D6\u6216\u5199\u5165\
  \uFF0C\u786E\u4FDD\u6587\u4EF6\u548C\u76EE\u5F55\u64CD\u4F5C\u66F4\u52A0\u987A\u7545\
  \u3002"
lastmod: '2024-03-13T22:44:47.783619-06:00'
model: gpt-4-0125-preview
summary: "\u5728 C# \u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u6D89\u53CA\
  \u9A8C\u8BC1\u6587\u4EF6\u7CFB\u7EDF\u4E2D\u6307\u5B9A\u8DEF\u5F84\u5904\u7684\u6587\
  \u4EF6\u5939\u662F\u5426\u5B58\u5728\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\u9879\
  \u68C0\u67E5\u662F\u4E3A\u4E86\u907F\u514D\u9519\u8BEF\uFF0C\u4F8B\u5982\u5C1D\u8BD5\
  \u4ECE\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u4E2D\u8BFB\u53D6\u6216\u5199\u5165\uFF0C\
  \u786E\u4FDD\u6587\u4EF6\u548C\u76EE\u5F55\u64CD\u4F5C\u66F4\u52A0\u987A\u7545\u3002\
  ."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

## 如何操作:


### 使用 System.IO
C# 提供了 `System.IO` 命名空间，其中包含了 `Directory` 类，通过 `Exists` 方法直接检查目录是否存在。

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // 检查目录是否存在
        bool directoryExists = Directory.Exists(directoryPath);

        // 打印结果
        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**示例输出:**

```
Directory exists: False
```

如果目录在路径 `C:\ExampleDirectory` 处确实存在，输出将会是 `True`。

### 使用 System.IO.Abstractions 进行单元测试
当涉及到让你的代码可单元测试，特别是当它与文件系统交互时，`System.IO.Abstractions` 包是一个流行的选择。它允许你在测试中抽象化和模拟文件系统操作。以下是使用这种方法检查目录是否存在的方式：

首先，确保你已经安装了包：

```
Install-Package System.IO.Abstractions
```

然后，你可以将 `IFileSystem` 注入到你的类中并使用它来检查目录是否存在，这使得单元测试更加容易。

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**示例输出:**

```
Directory exists: False
```

这种方法使你的应用逻辑与直接访问文件系统解耦，使你的代码更加模块化、可测试和可维护。
