---
title:                "使用TOML"
date:                  2024-01-26T04:21:06.370461-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/working-with-toml.md"
---

{{< edit_this_page >}}

## 什么以及为什么?
使用Elixir处理TOML意味着使用Elixir解析和生成TOML（Tom的明显、最小语言）数据。程序员使用它来处理配置文件，因为TOML易于阅读、易于解析，并且能很好地映射到哈希数据结构上。

## 如何操作:
首先，将一个TOML解析器添加到你的mix依赖中。这个例子使用的是`toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

读取一个TOML文件:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

将Elixir数据转换为TOML:

```elixir
data = %{title: "TOML示例", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

示例输出:

```elixir
"title = \"TOML示例\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## 深入了解
TOML由GitHub的联合创始人Tom Preston-Werner创建，用于配置文件。它的设计比XML更直接，比YAML更简洁，同时保持了一致性。

替代品包括JSON、YAML和INI文件，每种都在人类可读性和数据结构兼容性方面有其权衡。TOML在清晰表示表格数据和嵌套分组数据方面表现出色。

在Elixir中，处理TOML依赖于解码和编码库，这些库将TOML字符串转换为Elixir映射，反之亦然。解析工作通过匹配TOML的语法规则并将其转换为Elixir的数据类型来完成。编码则相反，它将Elixir的数据类型映射回有效的TOML语法。

## 另见
- TOML语言：https://toml.io/zh/
- `toml-elixir` GitHub仓库：https://github.com/bitwalker/toml-elixir
- `toml-elixir`的Hex包详情：https://hex.pm/packages/toml_elixir