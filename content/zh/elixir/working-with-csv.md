---
title:                "处理CSV文件"
aliases:
- zh/elixir/working-with-csv.md
date:                  2024-02-03T19:19:40.457711-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理CSV文件"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

与 CSV（逗号分隔值）文件的工作包括从这些文件读取和向这些文件写入数据，这是数据导入/导出或简单存储解决方案需要的常见任务。程序员利用这种功能进行系统间的数据交换、快速数据编辑，或在轻量级且易于操作的数据格式有优势的情况下使用。

## 如何操作：

Elixir 凭借其强大的模式匹配和管道支持功能，即使在没有第三方库的情况下也可以高效处理 CSV 文件。然而，对于更高级的需求，`nimble_csv` 库是一个快速且直接的选择。

### 不使用外部库读取 CSV 文件

您可以使用 Elixir 的内置函数读取和解析 CSV 文件：

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# 示例使用
CSVReader.read_file("data.csv")
# 输出：[["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### 写入 CSV 文件

同样，向 CSV 文件写入数据：

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# 示例使用
data = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", data)
# 创建包含格式化为 CSV 的数据的 output.csv
```

### 使用 `nimble_csv`

对于更复杂的 CSV 处理，`nimble_csv` 提供了一个强大且灵活的方式来处理 CSV 数据。首先，将 `nimble_csv` 添加到 `mix.exs` 中的依赖项并运行 `mix deps.get`：

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

使用 `nimble_csv` 解析 CSV 数据：

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# 示例使用
MyCSVParser.parse("data.csv")
# 使用 nimble_csv 的输出可以根据定义进行自定义，但通常看起来像是列表中的列表或元组，具体取决于您如何设置解析器。
```

使用 `nimble_csv` 写入 CSV 数据需要手动将数据转换成适当的格式，然后写入文件，这与纯 Elixir 示例类似，但利用 `nimble_csv` 生成正确格式化的 CSV 行。

通过为您的任务复杂性选择适当的方法，您可以在 Elixir 中灵活而强大地处理 CSV 文件。
