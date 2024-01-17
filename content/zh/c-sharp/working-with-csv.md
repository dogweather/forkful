---
title:                "cscv 文件处理"
html_title:           "C#: cscv 文件处理"
simple_title:         "cscv 文件处理"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

What & Why?

CSV（逗号分隔值）是一种用于存储和交换数据的格式，在计算机编程中经常被使用。它使用逗号作为字段之间的分隔符，因此被称为“逗号分隔值”。程序员经常使用CSV来读取和写入数据，因为它是一种简单和方便的格式，可被大多数编程语言轻松处理。

How to:

```C#
// 创建一个CSV文件并写入数据
using System;
using System.IO;

namespace CSVExample
{
    class Program
    {
        static void Main(string[] args)
        {
            // 创建一个包含4列的数据表
            string[,] data = new string[,] { {"Name", "Age", "City", "Country"}, {"John", "25", "New York", "USA"}, {"Maria", "30", "London", "UK"}, {"Gao", "28", "Beijing", "China"} };

            // 创建CSV文件并写入数据
            using (StreamWriter writer = new StreamWriter("data.csv"))
            {
                for (int i = 0; i < data.GetLength(0); i++)
                {
                    string line = "";
                    for (int j = 0; j < data.GetLength(1); j++)
                    {
                        // 在每个值之间用逗号分隔
                        line += data[i, j] + ",";
                    }
                    writer.WriteLine(line.TrimEnd(',')); // 去除最后一个逗号并写入文件
                }
            }
        }
    }
}
```

输出:

```C#
Name,Age,City,Country
John,25,New York,USA
Maria,30,London,UK
Gao,28,Beijing,China
```

Deep Dive:

CSV最早用于电子表格软件的数据交换，但现在也被广泛应用于Web开发和数据库管理。它比其他格式如Excel和JSON更简单，因为它只使用文本字符来表示数据，而不需要复杂的编码。有时，CSV文件可能会带来一些挑战，比如处理嵌套数据或带有特殊字符的数据。此时，程序员可以尝试使用库来解析CSV文件，如CsvHelper或CsvReader，以简化操作。

See Also:

- [CsvHelper](https://joshclose.github.io/CsvHelper/) - 一个用于读取、写入和转换CSV文件的开源库。
- [CsvReader](https://www.codeproject.com/Articles/9258/A-Fast-CSV-Reader) - 一个高性能的CSV文件读取器。
- [CSV格式](https://www.w3school.com.cn/html/html_table.asp) - 在HTML中使用CSV格式创建数据表。