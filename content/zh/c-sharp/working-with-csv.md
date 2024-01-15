---
title:                "处理CSV 数据"
html_title:           "C#: 处理CSV 数据"
simple_title:         "处理CSV 数据"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## 为什么

CSV（逗号分隔值）是一种非常普遍的数据格式，用于存储和传输表格数据。如果您需要处理大量表格数据，那么学习如何使用CSV可以节省您大量的时间和精力。

## 如何操作CSV文件

```C#
// 使用CsvHelper库读取CSV文件
using (var reader = new StreamReader("file.csv"))
using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
{
    // 将CSV文件中的数据映射到Person对象中
    var records = csv.GetRecords<Person>().ToList();

    // 遍历每一行数据并输出到控制台
    foreach (var record in records)
    {
        Console.WriteLine("姓名: " + record.Name);
        Console.WriteLine("年龄: " + record.Age);
        Console.WriteLine("性别: " + record.Gender);
    }
}
```

输出示例：
```
姓名: 张三
年龄: 25
性别: 男
姓名: 李四
年龄: 30
性别: 男
姓名: 王五
年龄: 28
性别: 女
```

## 深入了解CSV

CSV文件通常包含许多列和行，因此在处理数据时要特别小心。您可以使用第三方库如CsvHelper来轻松读取和写入CSV文件。此外，您还可以使用LINQ（结构化查询语言）来筛选和操作CSV数据，使处理更加简单和高效。

## 参考资料

- [CsvHelper库文档](https://joshclose.github.io/CsvHelper/)
- [使用LINQ查询CSV数据](https://www.codeproject.com/Articles/9258/Using-LINQ-to-Query-CSV-Files)
- [CSV文件格式介绍](https://www.computerhope.com/issues/ch001356.htm)

## 请阅读

- [C#电子书：入门指南](https://www.runoob.com/csharp/csharp-tutorial.html)
- [C#视频教程](https://www.youtube.com/watch?v=GhQdlIFylQ8)
- [C#开发者社区](https://docs.microsoft.com/en-us/dotnet/csharp/)