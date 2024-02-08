---
title:                "检查目录是否存在"
aliases:
- zh/c-sharp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:04.975161-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?

在 C# 中检查目录是否存在涉及验证文件系统中指定路径处的文件夹是否存在。程序员进行这项检查是为了避免错误，例如尝试从不存在的目录中读取或写入，确保文件和目录操作更加顺畅。

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
