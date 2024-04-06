---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:40.457711-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir \u51ED\u501F\u5176\u5F3A\u5927\
  \u7684\u6A21\u5F0F\u5339\u914D\u548C\u7BA1\u9053\u652F\u6301\u529F\u80FD\uFF0C\u5373\
  \u4F7F\u5728\u6CA1\u6709\u7B2C\u4E09\u65B9\u5E93\u7684\u60C5\u51B5\u4E0B\u4E5F\u53EF\
  \u4EE5\u9AD8\u6548\u5904\u7406 CSV \u6587\u4EF6\u3002\u7136\u800C\uFF0C\u5BF9\u4E8E\
  \u66F4\u9AD8\u7EA7\u7684\u9700\u6C42\uFF0C`nimble_csv` \u5E93\u662F\u4E00\u4E2A\u5FEB\
  \u901F\u4E14\u76F4\u63A5\u7684\u9009\u62E9\u3002"
lastmod: '2024-04-05T22:38:46.556992-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir \u51ED\u501F\u5176\u5F3A\u5927\u7684\
  \u6A21\u5F0F\u5339\u914D\u548C\u7BA1\u9053\u652F\u6301\u529F\u80FD\uFF0C\u5373\u4F7F\
  \u5728\u6CA1\u6709\u7B2C\u4E09\u65B9\u5E93\u7684\u60C5\u51B5\u4E0B\u4E5F\u53EF\u4EE5\
  \u9AD8\u6548\u5904\u7406 CSV \u6587\u4EF6\u3002\u7136\u800C\uFF0C\u5BF9\u4E8E\u66F4\
  \u9AD8\u7EA7\u7684\u9700\u6C42\uFF0C`nimble_csv` \u5E93\u662F\u4E00\u4E2A\u5FEB\u901F\
  \u4E14\u76F4\u63A5\u7684\u9009\u62E9\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

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
