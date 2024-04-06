---
date: 2024-01-20 17:54:21.632336-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5386\u53F2\u4E0A\uFF0C\u8BFB\
  \u53D6\u6587\u4EF6\u662F\u4EFB\u4F55\u7F16\u7A0B\u4EFB\u52A1\u7684\u57FA\u7840\u3002\
  C# \u53D1\u5C55\u4E86\u591A\u79CD\u65B9\u6CD5\u6765\u5904\u7406\u6587\u672C\u6587\
  \u4EF6\uFF0C\u5982 `File.ReadAllText`, `File.ReadAllLines`, \u548C `StreamReader`\u3002\
  \u6BCF\u79CD\u65B9\u6CD5\u90FD\u6709\u5176\u7528\u9014\uFF1A`ReadAllText`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.995858-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5386\u53F2\u4E0A\uFF0C\u8BFB\u53D6\u6587\
  \u4EF6\u662F\u4EFB\u4F55\u7F16\u7A0B\u4EFB\u52A1\u7684\u57FA\u7840\u3002C# \u53D1\
  \u5C55\u4E86\u591A\u79CD\u65B9\u6CD5\u6765\u5904\u7406\u6587\u672C\u6587\u4EF6\uFF0C\
  \u5982 `File.ReadAllText`, `File.ReadAllLines`, \u548C `StreamReader`\u3002\u6BCF\
  \u79CD\u65B9\u6CD5\u90FD\u6709\u5176\u7528\u9014\uFF1A`ReadAllText` \u9002\u5408\
  \u5C0F\u6587\u4EF6\uFF0C`ReadAllLines` \u65B9\u4FBF\u5904\u7406\u6BCF\u884C\u6570\
  \u636E\uFF0C`StreamReader` \u6700\u8282\u7701\u5185\u5B58\uFF0C\u9002\u5408\u5927\
  \u6587\u4EF6\u3002`StreamReader` \u662F\u6D41\u5F0F\u8BFB\u53D6\uFF0C\u63A7\u5236\
  \u66F4\u7CBE\u7EC6\uFF0C\u4F46\u4E0D\u5982\u524D\u4E24\u8005\u7B80\u5355\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

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
