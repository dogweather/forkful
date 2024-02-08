---
title:                "阅读文本文件"
aliases:
- zh/c-sharp/reading-a-text-file.md
date:                  2024-01-20T17:54:21.632336-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)
在编程中，读取文本文件是指提取存储在文件中的数据。程序员这么做是为了处理存储的信息，如配置、日志或用户数据。

## How to: (如何操作：)
```C#
using System;
using System.IO;

class ReadTextFileExample
{
    static void Main()
    {
        string filePath = @"C:\example.txt";

        // 方法1：使用 File.ReadAllText 一次性读取全部内容
        string fileContent = File.ReadAllText(filePath);
        Console.WriteLine(fileContent);

        // 方法2：使用 File.ReadAllLines 读取所有行
        string[] fileLines = File.ReadAllLines(filePath);
        foreach (string line in fileLines)
        {
            Console.WriteLine(line); // 输出每行
        }

        // 方法3：使用 StreamReader 逐行读取
        using (StreamReader reader = new StreamReader(filePath))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line); // 输出当前行
            }
        }
    }
}
```

Sample output (示例输出):
```
Hello, world!
这是第二行。
最后一行!
```

## Deep Dive (深入探索)
历史上，读取文件是任何编程任务的基础。C# 发展了多种方法来处理文本文件，如 `File.ReadAllText`, `File.ReadAllLines`, 和 `StreamReader`。每种方法都有其用途：`ReadAllText` 适合小文件，`ReadAllLines` 方便处理每行数据，`StreamReader` 最节省内存，适合大文件。`StreamReader` 是流式读取，控制更精细，但不如前两者简单。

还有各种第三方库，如 `NPOI` 和 `ClosedXML` 提供了更多文本和文件处理的功能，尤其是针对特定格式，如 Excel。另外，异步读取文件 (`ReadAllTextAsync`, `ReadAllLinesAsync`, `StreamReader.ReadLineAsync`) 在处理大文件或网络资源时提高了性能。

在 .NET 5 和更新的版本中，有更多的异步方法来提高性能和响应性。由于文件 IO 操作可能会阻塞，因此利用 `async` 和 `await` 关键字可以避免阻塞 UI 线程，特别是在桌面或移动应用程序中。

## See Also (另见)
- Microsoft Docs - StreamReader: [https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- Microsoft Docs - File and FileInfo: [https://docs.microsoft.com/en-us/dotnet/api/system.io.file](https://docs.microsoft.com/en-us/dotnet/api/system.io.file)
- Asynchronous File I/O: [https://docs.microsoft.com/en-us/dotnet/standard/io/asynchronous-file-i-o](https://docs.microsoft.com/en-us/dotnet/standard/io/asynchronous-file-i-o)
