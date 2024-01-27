---
title:                "处理 CSV 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
CSV，即逗号分隔值，简单且广泛用于存储表格数据。程序员处理CSV是因为它的通用性强，轻松与Excel和数据库配合使用。

## How to: (如何操作)
读取CSV文件：
```C#
using System;
using System.IO;

class Program {
    static void Main() {
        var csvData = File.ReadAllLines("data.csv");
        foreach (var line in csvData) {
            var values = line.Split(',');
            Console.WriteLine($"Name: {values[0]}, Age: {values[1]}");
        }
    }
}
```
输出样例：
```
Name: John, Age: 30
Name: Jane, Age: 25
```

写入CSV文件：
```C#
using System;
using System.Collections.Generic;
using System.IO;

class Program {
    static void Main() {
        var people = new List<(string Name, int Age)>
        {
            ("John", 30),
            ("Jane", 25)
        };

        using (var writer = new StreamWriter("output.csv"))
        {
            foreach (var person in people) {
                var line = $"{person.Name},{person.Age}";
                writer.WriteLine(line);
            }
        }
    }
}
```
无需样例输出，创建了 `output.csv` 文件。

## Deep Dive (深入探索)
CSV出现于早期计算机，便于文本数据表示。XML和JSON是现代替代品，但CSV因其简洁性依然流行。C# 处理CSV可以直接使用字符串操作或依赖第三方库如`CsvHelper`。面对复杂CSV数据，考虑规范化处理，比如引号和逗号的数据封装，避免解析错误。

## See Also (参考资料)
- Microsoft C# 文档: https://docs.microsoft.com/zh-cn/dotnet/csharp/
- CsvHelper 库: https://joshclose.github.io/CsvHelper/
- CSV 规范详解: https://tools.ietf.org/html/rfc4180
