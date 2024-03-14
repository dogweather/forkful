---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:25.782216-07:00
description: "\u5728 C# \u4E2D\u7F16\u5199\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u4EE5\
  \u7F16\u7A0B\u65B9\u5F0F\u5728\u6587\u4EF6\u7CFB\u7EDF\u4E0A\u521B\u5EFA\u6216\u4FEE\
  \u6539\u6587\u672C\u6587\u4EF6 - \u8FD9\u662F\u8BB8\u591A\u5E94\u7528\u7A0B\u5E8F\
  \uFF08\u5982\u65E5\u5FD7\u8BB0\u5F55\u3001\u6570\u636E\u5BFC\u51FA\u6216\u914D\u7F6E\
  \u7BA1\u7406\uFF09\u7684\u57FA\u672C\u4EFB\u52A1\u3002\u7A0B\u5E8F\u5458\u6267\u884C\
  \u6B64\u64CD\u4F5C\u662F\u4E3A\u4E86\u5728\u4F1A\u8BDD\u4E4B\u95F4\u6301\u4E45\u4FDD\
  \u5B58\u6570\u636E\u3001\u8DE8\u7CFB\u7EDF\u5206\u4EAB\u4FE1\u606F\u6216\u5B58\u50A8\
  \u4EBA\u7C7B\u53EF\u8BFB\u7684\u8F93\u51FA\u3002"
lastmod: '2024-03-13T22:44:47.788306-06:00'
model: gpt-4-0125-preview
summary: "\u5728 C# \u4E2D\u7F16\u5199\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u4EE5\u7F16\
  \u7A0B\u65B9\u5F0F\u5728\u6587\u4EF6\u7CFB\u7EDF\u4E0A\u521B\u5EFA\u6216\u4FEE\u6539\
  \u6587\u672C\u6587\u4EF6 - \u8FD9\u662F\u8BB8\u591A\u5E94\u7528\u7A0B\u5E8F\uFF08\
  \u5982\u65E5\u5FD7\u8BB0\u5F55\u3001\u6570\u636E\u5BFC\u51FA\u6216\u914D\u7F6E\u7BA1\
  \u7406\uFF09\u7684\u57FA\u672C\u4EFB\u52A1\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u6B64\
  \u64CD\u4F5C\u662F\u4E3A\u4E86\u5728\u4F1A\u8BDD\u4E4B\u95F4\u6301\u4E45\u4FDD\u5B58\
  \u6570\u636E\u3001\u8DE8\u7CFB\u7EDF\u5206\u4EAB\u4FE1\u606F\u6216\u5B58\u50A8\u4EBA\
  \u7C7B\u53EF\u8BFB\u7684\u8F93\u51FA\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 C# 中编写文本文件涉及以编程方式在文件系统上创建或修改文本文件 - 这是许多应用程序（如日志记录、数据导出或配置管理）的基本任务。程序员执行此操作是为了在会话之间持久保存数据、跨系统分享信息或存储人类可读的输出。

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
