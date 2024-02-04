---
title:                "使用JSON进行编程"
date:                  2024-02-03T19:22:16.841176-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

处理JSON包括将JSON格式的字符串解析成Elixir可以操作的数据结构，以及将Elixir数据结构序列化回JSON字符串。这对于Web开发、APIs和配置文件来说至关重要，因为JSON是一种轻量级的、基于文本的、与语言无关的数据交换格式，因其简单性和易读性而广泛使用。

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
