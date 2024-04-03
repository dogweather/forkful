---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:07.508331-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728C#\u4E2D\u5904\u7406CSV\u6587\u4EF6\
  \u53EF\u4EE5\u901A\u8FC7`System.IO`\u547D\u540D\u7A7A\u95F4\u6765\u5B8C\u6210\u57FA\
  \u672C\u64CD\u4F5C\uFF0C\u5BF9\u4E8E\u66F4\u590D\u6742\u7684\u64CD\u4F5C\u6216\u5904\
  \u7406\u5927\u578B\u6587\u4EF6\uFF0C\u53EF\u80FD\u4F1A\u8003\u8651\u4F7F\u7528\u7B2C\
  \u4E09\u65B9\u5E93\u5982`CsvHelper`\u3002\u4EE5\u4E0B\u662F\u4F7F\u7528\u8FD9\u4E24\
  \u79CD\u65B9\u6CD5\u4ECECSV\u6587\u4EF6\u4E2D\u8BFB\u53D6\u548C\u5411CSV\u6587\u4EF6\
  \u5199\u5165\u7684\u793A\u4F8B\u3002 #."
lastmod: '2024-03-13T22:44:47.793529-06:00'
model: gpt-4-0125-preview
summary: "\u5728C#\u4E2D\u5904\u7406CSV\u6587\u4EF6\u53EF\u4EE5\u901A\u8FC7`System.IO`\u547D\
  \u540D\u7A7A\u95F4\u6765\u5B8C\u6210\u57FA\u672C\u64CD\u4F5C\uFF0C\u5BF9\u4E8E\u66F4\
  \u590D\u6742\u7684\u64CD\u4F5C\u6216\u5904\u7406\u5927\u578B\u6587\u4EF6\uFF0C\u53EF\
  \u80FD\u4F1A\u8003\u8651\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u5982`CsvHelper`\u3002\
  \u4EE5\u4E0B\u662F\u4F7F\u7528\u8FD9\u4E24\u79CD\u65B9\u6CD5\u4ECECSV\u6587\u4EF6\
  \u4E2D\u8BFB\u53D6\u548C\u5411CSV\u6587\u4EF6\u5199\u5165\u7684\u793A\u4F8B."
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

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
