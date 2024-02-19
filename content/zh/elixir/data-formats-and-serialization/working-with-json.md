---
aliases:
- /zh/elixir/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:16.841176-07:00
description: "\u5904\u7406JSON\u5305\u62EC\u5C06JSON\u683C\u5F0F\u7684\u5B57\u7B26\
  \u4E32\u89E3\u6790\u6210Elixir\u53EF\u4EE5\u64CD\u4F5C\u7684\u6570\u636E\u7ED3\u6784\
  \uFF0C\u4EE5\u53CA\u5C06Elixir\u6570\u636E\u7ED3\u6784\u5E8F\u5217\u5316\u56DEJSON\u5B57\
  \u7B26\u4E32\u3002\u8FD9\u5BF9\u4E8EWeb\u5F00\u53D1\u3001APIs\u548C\u914D\u7F6E\u6587\
  \u4EF6\u6765\u8BF4\u81F3\u5173\u91CD\u8981\uFF0C\u56E0\u4E3AJSON\u662F\u4E00\u79CD\
  \u8F7B\u91CF\u7EA7\u7684\u3001\u57FA\u4E8E\u6587\u672C\u7684\u3001\u4E0E\u8BED\u8A00\
  \u65E0\u5173\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u56E0\u5176\u7B80\u5355\
  \u6027\u548C\u6613\u8BFB\u6027\u800C\u5E7F\u6CDB\u4F7F\u7528\u3002"
lastmod: 2024-02-18 23:08:58.885053
model: gpt-4-0125-preview
summary: "\u5904\u7406JSON\u5305\u62EC\u5C06JSON\u683C\u5F0F\u7684\u5B57\u7B26\u4E32\
  \u89E3\u6790\u6210Elixir\u53EF\u4EE5\u64CD\u4F5C\u7684\u6570\u636E\u7ED3\u6784\uFF0C\
  \u4EE5\u53CA\u5C06Elixir\u6570\u636E\u7ED3\u6784\u5E8F\u5217\u5316\u56DEJSON\u5B57\
  \u7B26\u4E32\u3002\u8FD9\u5BF9\u4E8EWeb\u5F00\u53D1\u3001APIs\u548C\u914D\u7F6E\u6587\
  \u4EF6\u6765\u8BF4\u81F3\u5173\u91CD\u8981\uFF0C\u56E0\u4E3AJSON\u662F\u4E00\u79CD\
  \u8F7B\u91CF\u7EA7\u7684\u3001\u57FA\u4E8E\u6587\u672C\u7684\u3001\u4E0E\u8BED\u8A00\
  \u65E0\u5173\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u56E0\u5176\u7B80\u5355\
  \u6027\u548C\u6613\u8BFB\u6027\u800C\u5E7F\u6CDB\u4F7F\u7528\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
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
