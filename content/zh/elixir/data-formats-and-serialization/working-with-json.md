---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:16.841176-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Elixir\u4E2D\uFF0C\u4F60\u53EF\u4EE5\
  \u4F7F\u7528`Jason`\u5E93\uFF0C\u8FD9\u662F\u4E00\u4E2A\u7528\u4E8EJSON\u89E3\u6790\
  \u548C\u751F\u6210\u7684\u6D41\u884C\u9009\u62E9\u3002\u9996\u5148\uFF0C\u5728`mix.exs`\u4E2D\
  \u5C06`Jason`\u6DFB\u52A0\u5230\u9879\u76EE\u7684\u4F9D\u8D56\u9879\u4E2D\uFF1A."
lastmod: '2024-04-05T22:38:46.555742-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Elixir\u4E2D\uFF0C\u4F60\u53EF\u4EE5\
  \u4F7F\u7528`Jason`\u5E93\uFF0C\u8FD9\u662F\u4E00\u4E2A\u7528\u4E8EJSON\u89E3\u6790\
  \u548C\u751F\u6210\u7684\u6D41\u884C\u9009\u62E9\u3002\u9996\u5148\uFF0C\u5728`mix.exs`\u4E2D\
  \u5C06`Jason`\u6DFB\u52A0\u5230\u9879\u76EE\u7684\u4F9D\u8D56\u9879\u4E2D\uFF1A."
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

## 如何操作：
在Elixir中，你可以使用`Jason`库，这是一个用于JSON解析和生成的流行选择。首先，在`mix.exs`中将`Jason`添加到项目的依赖项中：

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

然后，运行`mix deps.get`来获取依赖项。

### 解析JSON：
将JSON字符串转换为Elixir数据结构：

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# 输出： %{"name" => "John", "age" => 30}
```

### 生成JSON：
将Elixir映射转换成JSON字符串：

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# 输出： {"age":25,"name":"Jane"}
```

### 使用Structs：
要编码一个Elixir结构，你必须为你的结构实现`Jason.Encoder`协议。这里有一个例子：

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# 输出： {"age":28,"name":"Mike"}
```

这种简单的方法将帮助你开始将JSON处理集成到你的Elixir应用程序中，促进在各种编程环境中的数据交换。
