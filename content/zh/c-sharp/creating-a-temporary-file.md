---
title:                "C#: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么
今天我们将讨论如何在C#中创建临时文件。临时文件是在计算机上创建的文件，通常用于存储临时数据，在完成任务后被删除。它们在编程中非常有用，因为它们可以在内存中保存数据，而不需要永久性地存储在磁盘中。

## 如何
我们可以使用C#的`Path`类和`File`类来创建临时文件。首先，我们需要在代码文件中导入这两个类。

```
using System.IO;
```

接下来，我们可以使用`GetTempFileName()`方法来创建临时文件，并将其保存在一个变量中。

```
string tempFile = Path.GetTempFileName();
```

接着，我们可以使用`WriteAllText()`方法来向临时文件写入文本内容。

```
File.WriteAllText(tempFile, "This is a temporary file.");
```

最后，我们可以使用`Delete()`方法来删除临时文件。

```
File.Delete(tempFile);
```

以下是完整的示例代码：

```
using System;
using System.IO;

namespace TempFileExample
{
    class Program
    {
        static void Main(string[] args)
        {
            string tempFile = Path.GetTempFileName();
            File.WriteAllText(tempFile, "This is a temporary file.");
            File.Delete(tempFile);
        }
    }
}
```

运行以上代码后，您将在计算机上创建一个临时文件，内容为"this is a temporary file."，随后被删除。

## 深入了解
临时文件的使用是非常常见的，因为它们可以帮助我们在编程中更有效地管理数据。有时，我们需要保存一些临时数据，在任务完成后，就不再需要这些数据了。在这种情况下，临时文件就派上了用场。

值得一提的是，临时文件并不是完全无用的。它们在某些操作系统的临时目录中也可能会得到保留，并在系统重启后仍然存在。因此，我们需要确保在不再需要这些临时文件时，及时对其进行删除。

## 参考链接
- [Microsoft Docs: System.IO.Path Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=netcore-3.1)
- [Microsoft Docs: System.IO.File Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [C# Corner: Working With Temporary Files In C#](https://www.c-sharpcorner.com/article/working-with-temporary-files-in-c-sharp/)