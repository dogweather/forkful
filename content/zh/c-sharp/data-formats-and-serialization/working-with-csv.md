---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:07.508331-07:00
description: "CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\u662F\u4E00\
  \u79CD\u5E38\u89C1\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u5B83\u4EE5\u7EAF\
  \u6587\u672C\u7684\u5F62\u5F0F\u8868\u793A\u8868\u683C\u6570\u636E\uFF0C\u4F7F\u7528\
  \u9017\u53F7\u6765\u5206\u9694\u5404\u4E2A\u503C\u3002\u7A0B\u5E8F\u5458\u5904\u7406\
  CSV\u6587\u4EF6\u4EE5\u4FBF\u5728\u5404\u79CD\u5E94\u7528\u7A0B\u5E8F\u548C\u670D\
  \u52A1\u4E4B\u95F4\u8F7B\u677E\u5BFC\u5165\u3001\u5BFC\u51FA\u548C\u64CD\u4F5C\u6570\
  \u636E\uFF0C\u56E0\u4E3A\u5B83\u662F\u4E00\u79CD\u7B80\u5355\u3001\u5E7F\u6CDB\u652F\
  \u6301\u7684\u683C\u5F0F\uFF0C\u517C\u5BB9\u4E8E\u7535\u5B50\u8868\u683C\u5E94\u7528\
  \u7A0B\u5E8F\u3001\u6570\u636E\u5E93\u548C\u7F16\u7A0B\u8BED\u8A00\u3002"
lastmod: '2024-03-11T00:14:21.579586-06:00'
model: gpt-4-0125-preview
summary: "CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\u662F\u4E00\u79CD\
  \u5E38\u89C1\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u5B83\u4EE5\u7EAF\u6587\
  \u672C\u7684\u5F62\u5F0F\u8868\u793A\u8868\u683C\u6570\u636E\uFF0C\u4F7F\u7528\u9017\
  \u53F7\u6765\u5206\u9694\u5404\u4E2A\u503C\u3002\u7A0B\u5E8F\u5458\u5904\u7406CSV\u6587\
  \u4EF6\u4EE5\u4FBF\u5728\u5404\u79CD\u5E94\u7528\u7A0B\u5E8F\u548C\u670D\u52A1\u4E4B\
  \u95F4\u8F7B\u677E\u5BFC\u5165\u3001\u5BFC\u51FA\u548C\u64CD\u4F5C\u6570\u636E\uFF0C\
  \u56E0\u4E3A\u5B83\u662F\u4E00\u79CD\u7B80\u5355\u3001\u5E7F\u6CDB\u652F\u6301\u7684\
  \u683C\u5F0F\uFF0C\u517C\u5BB9\u4E8E\u7535\u5B50\u8868\u683C\u5E94\u7528\u7A0B\u5E8F\
  \u3001\u6570\u636E\u5E93\u548C\u7F16\u7A0B\u8BED\u8A00\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么是CSV以及为什么使用它？
CSV（逗号分隔值）文件是一种常见的数据交换格式，它以纯文本的形式表示表格数据，使用逗号来分隔各个值。程序员处理CSV文件以便在各种应用程序和服务之间轻松导入、导出和操作数据，因为它是一种简单、广泛支持的格式，兼容于电子表格应用程序、数据库和编程语言。

## 如何操作：
在C#中处理CSV文件可以通过`System.IO`命名空间来完成基本操作，对于更复杂的操作或处理大型文件，可能会考虑使用第三方库如`CsvHelper`。以下是使用这两种方法从CSV文件中读取和向CSV文件写入的示例。

### 使用System.IO读取CSV文件
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"路径\到\你的\文件.csv";
        // 读取CSV文件的所有行
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"第一列：{rowData[0]}，第二列：{rowData[1]}");
        }
    }
}
```

**示例输出：**
```
第一列：姓名，第二列：年龄
第一列：约翰·多，第二列：30
```

### 使用System.IO写入CSV文件
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"路径\到\你的\输出文件.csv";
        var lines = new List<string>
        {
            "姓名,年龄",
            "约翰·多,30",
            "简·史密斯,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("已写入CSV文件。");
    }
}
```

**示例输出：**
```
已写入CSV文件。
```

### 使用CsvHelper读取CSV
首先，使用NuGet包管理器将`CsvHelper`包添加到你的项目中。

```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Linq;
using CsvHelper.Configuration;

class ReadCSVWithCsvHelper
{
    static void Main()
    {
        string filePath = @"路径\到\你的\文件.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"第一列：{record.Name}，第二列：{record.Age}");
            }
        }
    }
}
```

**示例输出：**
```
第一列：约翰·多，第二列：30
第一列：简·史密斯，第二列：25
```

### 使用CsvHelper写入CSV
```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Collections.Generic;
using CsvHelper.Configuration;

class WriteCSVWithCsvHelper
{
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }

    static void Main()
    {
        string filePath = @"路径\到\你的\输出文件.csv";
        var records = new List<Person>
        {
            new Person { Name = "约翰·多", Age = 30 },
            new Person { Name = "简·史密斯", Age = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("使用CsvHelper写入CSV文件。");
    }
}
```

**示例输出：**
```
使用CsvHelper写入CSV文件。
```
