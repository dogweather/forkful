---
title:                "从字符串解析日期"
date:                  2024-01-20T15:35:40.526908-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么？)

把字符串转化为日期是一个常见的编程任务。程序员这么做是为了可以对日期进行操作和计算。

## How to: (怎么做：)

在Elixir中，你可以用`Timex`这个库来解析日期字符串：

```elixir
# 先引入依赖Timex
{:timex, "~> 3.7"}

# 使用Timex解析日期字符串
def parse_date_string(date_string) do
  {:ok, datetime} = Timex.parse(date_string, "{YYYY}-{0M}-{0D}")
  datetime
end

# 示例
parse_date_string("2023-04-01")
```

```output
# 运行结果将会是这样：
# %DateTime{calendar: Calendar.ISO, day: 1, hour: 0, minute: 0, month: 4, second: 0, year: 2023, ...}
```

## Deep Dive (深入探索)

Elixir没有内置的日期字符串解析，因此经常用`Timex`。`Timex`是个强大的库，从历史上看，它增加了Elixir处理时间和日期的能力。虽然使用`DateTime.from_iso8601/2`也能解析某些标准格式字符串，但`Timex`提供了更灵活的解析选项。

```elixir
# 用Elixir内置的方法解析ISO 8601格式日期
DateTime.from_iso8601("2023-04-01T00:00:00Z")
```

```output
# 运行结果：
# {:ok, %DateTime{calendar: Calendar.ISO, ...}, 0}
```

`Timex`用Elixir原生态来扩展功能，它支持多种格式，包括自定义格式，也提供了本地化和时区转换的更多功能。

## See Also (另请参见)

- [Timex GitHub repository](https://github.com/bitwalker/timex) - `Timex`库的源代码和文档。
- [Elixir DateTime module](https://hexdocs.pm/elixir/DateTime.html) - Elixir官方文档关于日期时间模块的说明。
