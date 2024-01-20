---
title:                "处理 CSV 文件"
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? 什么是CSV以及为何使用它?
CSV是用逗号分隔的值(comma-separated values)的简称，常用于存储和交换简单结构的数据。程序员使用CSV是因为它易于阅读，且容易被不同的系统和程序所处理。

## How to: 如何操作
在Gleam中使用CSV，可以创建和读取数据。下面是示例代码：

```Gleam
// 引入CSV处理库
import gleam/csv

// 解析CSV字符串
pub fn parse_csv(data: String) {
  let rows = csv.decode(data)
  case rows {
    Ok(rows) -> rows
    Error(error) -> []
  }
}

// 生成CSV字符串
pub fn generate_csv(data: List(List(String))) {
  csv.encode(data)
}

// 示例
fn main() {
  let data = "name,age\nAlice,30\nBob,25"
  let parsed_data = parse_csv(data)
  let generated_csv = generate_csv(parsed_data)
  parsed_data |> io.debug // 打印解析后的数据
  generated_csv |> io.debug // 打印生成的CSV字符串
}
```

示例输出:

```
[["name", "age"], ["Alice", "30"], ["Bob", "25"]]
"name,age\nAlice,30\nBob,25"
```

## Deep Dive 深入探讨
CSV格式起源于早期计算机，便于在不同应用间传输表格数据。虽然现在有JSON、XML等替代格式，但CSV因其简单性仍被广泛使用。在Gleam中，`gleam/csv`库提供了基本的CSV编解码功能。Gleam是一个静态类型的函数式编程语言，它的CSV处理旨在保持代码的安全性和简洁性。

## See Also 相关资料
- Gleam官方文档: [https://gleam.run/](https://gleam.run/)
- CSV格式介绍: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)