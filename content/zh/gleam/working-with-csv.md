---
title:                "使用 csv 进行编程"
html_title:           "Gleam: 使用 csv 进行编程"
simple_title:         "使用 csv 进行编程"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

Gleam编程指南：CSV文件操作

## 什么是CSV？为什么程序员需要它？

CSV是一种常见的文件格式，用于存储和传输数据。它是一种简单的文本格式，由逗号分隔的值组成，通常用于存储表格数据。程序员经常使用CSV文件来读取和写入数据，因为它们易于处理和分析。

## 如何操作CSV文件？

```Gleam
import gleam/csv

// 从CSV文件读取数据
let data = csv.from_file("data.csv")

// 选择特定的列
let selected_data = csv.select(data, ["name", "age"])

// 将数据写入CSV文件
csv.to_file("new_data.csv", selected_data)
```

输出：

```
name,age
John,25
Amy,30
```

## 深入了解CSV文件

CSV文件最初是由Microsoft Excel开发的，但现在已经成为一种普遍的数据交换格式。除了逗号，它还可以使用其他分隔符，如制表符或分号。另外，CSV文件的编码格式也可能不同，需要根据实际情况选择合适的编码方式。

除了使用Gleam提供的CSV库外，还可以使用其他编程语言中的库来操作CSV文件，如Python中的csv模块和Java中的Apache Commons CSV。

## 相关资源

- Gleam官方文档：https://gleam.run/
- CSV文件的历史和标准规范：https://tools.ietf.org/html/rfc4180
- Python中的CSV模块文档：https://docs.python.org/3/library/csv.html
- Java中的Apache Commons CSV文档：https://commons.apache.org/proper/commons-csv/