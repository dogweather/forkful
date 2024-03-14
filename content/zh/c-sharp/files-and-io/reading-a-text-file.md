---
date: 2024-01-20 17:54:21.632336-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u662F\
  \u6307\u63D0\u53D6\u5B58\u50A8\u5728\u6587\u4EF6\u4E2D\u7684\u6570\u636E\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\u7406\u5B58\u50A8\u7684\u4FE1\
  \u606F\uFF0C\u5982\u914D\u7F6E\u3001\u65E5\u5FD7\u6216\u7528\u6237\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.787045-06:00'
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u662F\
  \u6307\u63D0\u53D6\u5B58\u50A8\u5728\u6587\u4EF6\u4E2D\u7684\u6570\u636E\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\u7406\u5B58\u50A8\u7684\u4FE1\
  \u606F\uFF0C\u5982\u914D\u7F6E\u3001\u65E5\u5FD7\u6216\u7528\u6237\u6570\u636E\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
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
