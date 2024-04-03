---
date: 2024-01-26 04:21:06.370461-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u9996\u5148\uFF0C\u5C06\u4E00\u4E2ATOML\u89E3\
  \u6790\u5668\u6DFB\u52A0\u5230\u4F60\u7684mix\u4F9D\u8D56\u4E2D\u3002\u8FD9\u4E2A\
  \u4F8B\u5B50\u4F7F\u7528\u7684\u662F`toml-elixir`."
lastmod: '2024-03-13T22:44:47.393701-06:00'
model: gpt-4-0125-preview
summary: "\u9996\u5148\uFF0C\u5C06\u4E00\u4E2ATOML\u89E3\u6790\u5668\u6DFB\u52A0\u5230\
  \u4F60\u7684mix\u4F9D\u8D56\u4E2D\u3002\u8FD9\u4E2A\u4F8B\u5B50\u4F7F\u7528\u7684\
  \u662F`toml-elixir`."
title: "\u4F7F\u7528TOML"
weight: 39
---

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
