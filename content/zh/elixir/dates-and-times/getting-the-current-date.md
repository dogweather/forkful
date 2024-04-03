---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:39.059709-07:00
description: "\u5728 Elixir \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\u53CA\
  \u8BBF\u95EE\u7CFB\u7EDF\u7684\u65E5\u671F\u548C\u65F6\u95F4\u4FE1\u606F\uFF0C\u8FD9\
  \u662F\u8BB0\u5F55\u3001\u6570\u636E\u6807\u8BB0\u6216\u4EFB\u4F55\u9700\u8981\u4E86\
  \u89E3\u5F53\u524D\u65E5\u671F\u7684\u529F\u80FD\u7684\u5E38\u89C1\u4EFB\u52A1\u3002\
  \u8FD9\u4E00\u64CD\u4F5C\u5BF9\u4E8E\u521B\u5EFA\u65F6\u95F4\u611F\u77E5\u7684\u5E94\
  \u7528\u7A0B\u5E8F\u4EE5\u53CA\u751F\u6210\u62A5\u544A\u6216\u5728 Web \u5E94\u7528\
  \u7A0B\u5E8F\u4E2D\u751F\u6210\u65F6\u95F4\u6233\u7B49\u4EFB\u52A1\u81F3\u5173\u91CD\
  \u8981\u3002"
lastmod: '2024-03-13T22:44:47.378082-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Elixir \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\u53CA\u8BBF\
  \u95EE\u7CFB\u7EDF\u7684\u65E5\u671F\u548C\u65F6\u95F4\u4FE1\u606F\uFF0C\u8FD9\u662F\
  \u8BB0\u5F55\u3001\u6570\u636E\u6807\u8BB0\u6216\u4EFB\u4F55\u9700\u8981\u4E86\u89E3\
  \u5F53\u524D\u65E5\u671F\u7684\u529F\u80FD\u7684\u5E38\u89C1\u4EFB\u52A1\u3002\u8FD9\
  \u4E00\u64CD\u4F5C\u5BF9\u4E8E\u521B\u5EFA\u65F6\u95F4\u611F\u77E5\u7684\u5E94\u7528\
  \u7A0B\u5E8F\u4EE5\u53CA\u751F\u6210\u62A5\u544A\u6216\u5728 Web \u5E94\u7528\u7A0B\
  \u5E8F\u4E2D\u751F\u6210\u65F6\u95F4\u6233\u7B49\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\
  \u3002."
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
