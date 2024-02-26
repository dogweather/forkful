---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:40.457711-07:00
description: "\u4E0E CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\u7684\
  \u5DE5\u4F5C\u5305\u62EC\u4ECE\u8FD9\u4E9B\u6587\u4EF6\u8BFB\u53D6\u548C\u5411\u8FD9\
  \u4E9B\u6587\u4EF6\u5199\u5165\u6570\u636E\uFF0C\u8FD9\u662F\u6570\u636E\u5BFC\u5165\
  /\u5BFC\u51FA\u6216\u7B80\u5355\u5B58\u50A8\u89E3\u51B3\u65B9\u6848\u9700\u8981\u7684\
  \u5E38\u89C1\u4EFB\u52A1\u3002\u7A0B\u5E8F\u5458\u5229\u7528\u8FD9\u79CD\u529F\u80FD\
  \u8FDB\u884C\u7CFB\u7EDF\u95F4\u7684\u6570\u636E\u4EA4\u6362\u3001\u5FEB\u901F\u6570\
  \u636E\u7F16\u8F91\uFF0C\u6216\u5728\u8F7B\u91CF\u7EA7\u4E14\u6613\u4E8E\u64CD\u4F5C\
  \u7684\u6570\u636E\u683C\u5F0F\u6709\u4F18\u52BF\u7684\u60C5\u51B5\u4E0B\u4F7F\u7528\
  \u3002"
lastmod: '2024-02-25T18:49:45.012982-07:00'
model: gpt-4-0125-preview
summary: "\u4E0E CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\u7684\u5DE5\
  \u4F5C\u5305\u62EC\u4ECE\u8FD9\u4E9B\u6587\u4EF6\u8BFB\u53D6\u548C\u5411\u8FD9\u4E9B\
  \u6587\u4EF6\u5199\u5165\u6570\u636E\uFF0C\u8FD9\u662F\u6570\u636E\u5BFC\u5165/\u5BFC\
  \u51FA\u6216\u7B80\u5355\u5B58\u50A8\u89E3\u51B3\u65B9\u6848\u9700\u8981\u7684\u5E38\
  \u89C1\u4EFB\u52A1\u3002\u7A0B\u5E8F\u5458\u5229\u7528\u8FD9\u79CD\u529F\u80FD\u8FDB\
  \u884C\u7CFB\u7EDF\u95F4\u7684\u6570\u636E\u4EA4\u6362\u3001\u5FEB\u901F\u6570\u636E\
  \u7F16\u8F91\uFF0C\u6216\u5728\u8F7B\u91CF\u7EA7\u4E14\u6613\u4E8E\u64CD\u4F5C\u7684\
  \u6570\u636E\u683C\u5F0F\u6709\u4F18\u52BF\u7684\u60C5\u51B5\u4E0B\u4F7F\u7528\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
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
