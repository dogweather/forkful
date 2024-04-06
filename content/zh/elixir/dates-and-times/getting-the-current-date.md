---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:39.059709-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u901A\u8FC7 `DateTime` \u6A21\u5757\uFF0C\
  Elixir \u7684\u6807\u51C6\u5E93\u5141\u8BB8\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u548C\
  \u65F6\u95F4\u3002\u7531\u4E8E Elixir \u8FD0\u884C\u5728 Erlang VM (BEAM) \u4E0A\
  \uFF0C\u5B83\u5229\u7528\u4E86\u5E95\u5C42 Erlang \u5BF9\u65F6\u95F4\u64CD\u4F5C\
  \u7684\u529F\u80FD\u3002"
lastmod: '2024-04-05T21:53:47.711276-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

## 如何操作：
通过 `DateTime` 模块，Elixir 的标准库允许获取当前日期和时间。由于 Elixir 运行在 Erlang VM (BEAM) 上，它利用了底层 Erlang 对时间操作的功能。

### 使用 Elixir 的标准库
Elixir 提供 `DateTime.utc_now/0` 函数来获取当前的日期和时间（UTC）。

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**示例输出：**
```
~U[2024-02-05 19:58:40.925931Z]
```

要获取当前的日期，您可以提取年、月、日组件：

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**示例输出：**
```
~D[2023-05-04]
```

### 使用 Timex 库
对于更复杂的日期时间需求，可以使用一个受欢迎的第三方库 Timex。首先，将 `Timex` 添加到您的 mix.exs 依赖中：

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

安装依赖项（`mix deps.get`）后，您可以使用 Timex 获取当前日期：

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**示例输出：**
```
~D[2023-05-04]
```

Timex 提供了丰富的日期时间操纵功能，使其成为您的 Elixir 应用程序的强大补充，特别是在处理时区、格式化和解析日期和时间时。

通过了解和利用 Elixir 的内置功能和 Timex 库，您可以轻松地在您的 Elixir 应用程序中处理日期和时间，根据您应用程序的需求精确而轻松地定制体验。
