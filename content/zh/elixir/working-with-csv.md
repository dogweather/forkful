---
title:                "处理 CSV 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
CSV（逗号分隔值）是一种简单的文件格式，用来存储表格数据。程序员之所以用它，是因为它被广泛支持，易于阅读和编辑，尤其适合数据导入导出。

## How to: (怎么做)
首先要安装CSV解析库，用`mix`命令：

```elixir
# 在命令行中运行
mix deps.get
```

接下来，用Elixir读写CSV示例：

```elixir
# 引入CSV库
import CSV

# 读CSV文件
def read_csv(file_path) do
  file_path
  |> File.stream!()
  |> CSV.decode(headers: true)
  |> Enum.each(fn row -> IO.inspect(row) end)
end

# 写CSV文件
def write_csv(file_path, data) do
  CSV.encode_to_iodata(data)
  |> :file.write(file_path)
end
```

执行`read_csv/1`读CSV：

```elixir
read_csv("your_file.csv")
# 输出示例：[%{"name" => "John", "age" => "42"}]
```

执行`write_csv/2`写CSV：

```elixir
write_csv("output.csv", [["name", "age"], ["Jane", "34"]])
# 在output.csv内将看到
# name,age
# Jane,34
```

## Deep Dive (深入探究)
CSV历史久远，起始于1970年代早期的电子表格程序。它简化了与Excel或数据库的数据交换。不止有CSV，还有如JSON、XML这样的数据格式。在Elixir中处理CSV，我们通常使用第三方库，例如CSV库，也有别的库如NimbleCSV。这些库处理细节、性能优化和错误管理。

## See Also (另请参阅)
- [Elixir CSV库](https://hex.pm/packages/csv)
- [Elixir 官方文档](https://elixir-lang.org/docs.html)
- [CSV Wikipedia页面](https://zh.wikipedia.org/wiki/逗号分隔值)
