---
title:    "C#: 查询目录是否存在"
keywords: ["C#"]
---

{{< edit_this_page >}}

# 为什么要检查目录是否存在

在进行C＃编程时，经常会涉及到检查某个目录是否存在的任务。这样做的原因可能是为了验证用户输入的文件路径是否正确，或者为了避免程序发生错误而导致程序崩溃。 在这篇博客文章中，我们将讨论如何在C＃中检查目录是否存在，并深入了解这个过程的背后原理。

## 如何进行检查

在C＃中，我们可以使用System.IO命名空间中的Directory类来检查目录是否存在。我们可以使用Exists()方法，它将返回一个bool值来指示目录是否存在。下面是一个简单的示例代码，展示了如何使用Exists()方法来检查一个名为“test”的目录是否存在。

```C#
using System;
using System.IO;

namespace CheckDirectoryExistence
{
    class Program
    {
        static void Main(string[] args)
        {
            // 创建一个目录
            Directory.CreateDirectory("test");

            // 检查目录是否存在
            if (Directory.Exists("test"))
            {
                Console.WriteLine("目录存在！");
            }
            else
            {
                Console.WriteLine("目录不存在！");
            }
        }
    }
}
```

运行上面的代码，我们将得到以下输出：

```
目录存在！
```

## 深入了解

在上面的代码中，我们使用了Directory类的Exists()方法来检查目录是否存在。但实际上，这个方法背后的原理却并不简单。它需要调用操作系统的API来获取有关目录的信息，并返回一个bool值。如果存在这个目录，那么根据操作系统的不同，它可能会从缓存中获取该信息，使得速度更快。 但是，如果目录不存在，系统将会访问磁盘来获取该信息，这可能会导致性能下降。

此外，当我们检查一个目录是否存在的时候，实际上是在检查该路径是否具有合适的权限。如果我们没有权限访问该目录，那么即使它已经存在，Exists()方法也会返回false。

## 另请参阅

- [MSDN 文档 - Directory.Exists() 方法](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [MSDN 文档 - System.IO 命名空间](https://docs.microsoft.com/en-us/dotnet/api/system.io)
- [C# Dot Net Perls - Checking if a Directory Exists](https://www.dotnetperls.com/directory-exists)

# 同样查看

- [如何在C＃中创建目录](https://www.example.com/how-to-create-directory-c-sharp)
- [如何在C＃中删除目录](https://www.example.com/how-to-delete-directory-c-sharp)
- [如何在C＃中遍历目录内容](https://www.example.com/how-to-traverse-directory-c-sharp)