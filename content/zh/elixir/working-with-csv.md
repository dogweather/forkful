---
title:                "与CSV操作"
html_title:           "Elixir: 与CSV操作"
simple_title:         "与CSV操作"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## CSV 是什么 & 为什么要使用?
CSV 是一种常用的文件格式，用于存储数据并与其他软件进行交互时使用。它采用逗号分隔值（Comma-Separated Values）的格式，将数据以表格形式排列，每一行为一条记录，每一列为不同的数据字段。程序员们通过处理 CSV 文件来读取、写入和修改数据，使得他们可以更有效地处理大量数据。

## 如何实现:
通过使用 Elixir 内置的 CSV 库，我们可以轻松地处理 CSV 文件。首先，我们需要导入 `:csv` 模块，然后使用 `:csv.decode` 函数来读取 CSV 文件并将其转换为 Elixir 中的数据结构。例如:
```Elixir
require CSV
{:ok, data} = :csv.decode(File.read!("data.csv"))
```
之后，我们可以使用 `data` 变量来访问 CSV 文件中的数据。如果要将数据写入 CSV 文件，我们可以使用 `:csv.encode` 函数来将数据转换为 CSV 格式，然后使用 `File.write!` 函数将数据写入文件中。例如:
```Elixir
:csv.encode(data)
|> File.write!("output.csv")
```

## 深入了解:
CSV 最早是由微软在 1983 年创建，将此格式作为 Excel 文档的一部分。目前，它已经成为处理数据的标准格式，广泛用于数据库、电子表格等软件中。除了 Elixir 内置的 CSV 库，还有一些其他的库也支持 CSV 文件的读写，例如 `:csv_parser` 和 `nimble_csv`。此外，CSV 文件可以包含各种各样的数据类型，因此在处理时要小心处理，避免出现错误。

## 参考资料:
- Elixir's CSV 模块文档: [https://hexdocs.pm/elixir/CSV.html](https://hexdocs.pm/elixir/CSV.html)
- CSV 文件的历史: [https://en.wikipedia.org/wiki/Comma-separated_values](https://en.wikipedia.org/wiki/Comma-separated_values)
- Elixir CSV 库的源码: [https://github.com/elixir-lang/elixir/blob/master/lib/csv/](https://github.com/elixir-lang/elixir/blob/master/lib/csv/)