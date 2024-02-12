---
title:                "处理CSV文件"
aliases:
- zh/c-sharp/working-with-csv.md
date:                  2024-02-03T19:20:07.508331-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理CSV文件"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
