---
title:                "获取当前日期"
aliases:
- zh/elixir/getting-the-current-date.md
date:                  2024-02-03T19:09:39.059709-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
  - 2024-02-05, dogweather, reviewed and corrected
lastmod:              2024-02-05
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Elixir 中获取当前日期涉及访问系统的日期和时间信息，这是记录、数据标记或任何需要了解当前日期的功能的常见任务。这一操作对于创建时间感知的应用程序以及生成报告或在 Web 应用程序中生成时间戳等任务至关重要。

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
