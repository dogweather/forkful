---
title:                "检查目录是否存在"
date:                  2024-01-19
html_title:           "Bash: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么？)
检查目录是否存在是查询文件系统，确认指定路径的文件夹是否已经存在的过程。程序员这么做是为了避免读写错误，确保文件操作针对的是正确的路径。

## How to (如何执行)
C#中可以使用`System.IO`命名空间下的`Directory`类来检查目录是否存在。这里是一个示例：

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string pathToCheck = @"C:\MyFolder";

        if (Directory.Exists(pathToCheck))
        {
            Console.WriteLine("Directory exists.");
        }
        else
        {
            Console.WriteLine("Directory does not exist.");
        }
    }
}
```

如果目录存在，控制台输出：

```
Directory exists.
```

如果目录不存在，控制台输出：

```
Directory does not exist.
```

## Deep Dive (深入了解)
早期的.NET版本就已经引入了`System.IO.Directory.Exists`方法，至今依然是检查目录存在性的简洁方式。作为替代，也可以通过异常处理来尝试访问目录并捕获可能的`DirectoryNotFoundException`，但这通常认为是不推荐的做法，因为异常处理代价较高。具体实现是通过调用系统级别的文件访问API来确定目录是否存在。

## See Also (参考链接)
- Microsoft Docs on `Directory.Exists`: [https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- Stack Overflow discussion on checking directory existence: [https://stackoverflow.com/questions/1410127/c-sharp-test-if-user-has-write-access-to-a-folder](https://stackoverflow.com/questions/1410127/c-sharp-test-if-user-has-write-access-to-a-folder)
