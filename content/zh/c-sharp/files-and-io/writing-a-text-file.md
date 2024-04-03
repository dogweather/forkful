---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:25.782216-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A C# \u901A\u8FC7\u5176 `System.IO` \u547D\
  \u540D\u7A7A\u95F4\u7B80\u5316\u4E86\u6587\u4EF6\u64CD\u4F5C\uFF0C\u63D0\u4F9B\u4E86\
  \u76F4\u63A5\u7684\u65B9\u6CD5\u6765\u7F16\u5199\u6587\u672C\u6587\u4EF6\u3002\u4EE5\
  \u4E0B\u662F\u7F16\u5199\u57FA\u672C\u6587\u672C\u6587\u4EF6\u548C\u5411\u73B0\u6709\
  \u6587\u4EF6\u8FFD\u52A0\u6587\u672C\u7684\u65B9\u6CD5\u3002 #."
lastmod: '2024-03-13T22:44:47.788306-06:00'
model: gpt-4-0125-preview
summary: "C# \u901A\u8FC7\u5176 `System.IO` \u547D\u540D\u7A7A\u95F4\u7B80\u5316\u4E86\
  \u6587\u4EF6\u64CD\u4F5C\uFF0C\u63D0\u4F9B\u4E86\u76F4\u63A5\u7684\u65B9\u6CD5\u6765\
  \u7F16\u5199\u6587\u672C\u6587\u4EF6\u3002\u4EE5\u4E0B\u662F\u7F16\u5199\u57FA\u672C\
  \u6587\u672C\u6587\u4EF6\u548C\u5411\u73B0\u6709\u6587\u4EF6\u8FFD\u52A0\u6587\u672C\
  \u7684\u65B9\u6CD5."
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

## 如何操作：
C# 通过其 `System.IO` 命名空间简化了文件操作，提供了直接的方法来编写文本文件。以下是编写基本文本文件和向现有文件追加文本的方法。

### 从头开始向文本文件写入
```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "Hello, world!";

        // 将内容写入新文件
        File.WriteAllText(filePath, content);
        
        Console.WriteLine("文件写入成功。");
    }
}
```
**示例输出：**
```
文件写入成功。
```

### 向现有文件追加文本
如果你希望将文本添加到现有文件的末尾，可以使用 `File.AppendAllText` 方法。

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string additionalContent = "\n添加更多内容。";

        // 将内容追加到文件
        File.AppendAllText(filePath, additionalContent);
        
        Console.WriteLine("内容追加成功。");
    }
}
```
**示例输出：**
```
内容追加成功。
```

### 使用第三方库：`StreamWriter`
为了更细致地控制写入，包括自动刷新和编码选择，请使用 `StreamWriter`。

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "这是使用 StreamWriter 的示例。";

        // 使用 StreamWriter 写入文件
        using (StreamWriter writer = new StreamWriter(filePath, append: true))
        {
            writer.WriteLine(content);
        }
        
        Console.WriteLine("使用 StreamWriter 成功写入文件。");
    }
}
```
**示例输出：**
```
使用 StreamWriter 成功写入文件。
```

这些方法各自适用于不同的需求：直接使用 `File` 方法进行快速操作，使用 `StreamWriter` 进行更复杂的写入场景。根据您的具体需求选择，考虑性能和文件大小等因素。
