---
title:                "处理JSON数据"
date:                  2024-01-19
simple_title:         "处理JSON数据"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
### 什么 & 为什么?
JSON(JavaScript Object Notation) 是数据交换的轻量格式。因为它简单易读，所以程序员用JSON保存和传输数据。

## How to:
### 如何操作:
```elixir
# 安装Josn库
defp deps do
  [
    {:jason, "~> 1.2"}
  ]
end

# 读JSON
json = ~s({"name": "Alice", "job": "Engineer", "age": 30})
person = Jason.decode!(json)
IO.inspect(person) # 输出: %{"age" => 30, "job" => "Engineer", "name" => "Alice"}

# 写JSON
person_map = %{"name" => "Bob", "job" => "Designer", "age" => 28}
json_string = Jason.encode!(person_map)
IO.puts(json_string) # 输出: {"age":28,"job":"Designer","name":"Bob"}
```

## Deep Dive
### 深入探究
Elixir中处理JSON最常用的库是Jason和Poison。Jason是更现代的选择，性能更好。JSON是一种基于文本的格式，应对不同的编程语言传递数据。透过应对各种数据类型和结构，它成为网络应用程序之间通信的标准。

## See Also
### 另见
- [Jason GitHub](https://github.com/michalmuskala/jason)
- [Poison GitHub](https://github.com/devinus/poison)
