---
title:                "从字符串中解析日期"
html_title:           "Elixir: 从字符串中解析日期"
simple_title:         "从字符串中解析日期"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 是什么和为什么?

日期字符串解析是一种在编程中常见的任务，它涉及将一个日期储存在字符串中，然后将其转换为编程语言所能识别的日期格式。程序员通常会进行这样的操作，因为它可以使日期的操作更容易和有效，并且能够将日期与其他数据一起使用。

## 如何做:

```elixir
# 讲一个日期字符串解析为 Elixir 中的日期
Date.from_iso8601("2021-10-31")
# 输出: {:ok, ~D[2021-10-31]}

# 将一个日期字符串解析为 Elixir 中的时间
Time.from_iso8601("2021-10-31T12:00:00Z")
# 输出: {:ok, ~T[12:00:00Z]}

# 让日期更易于比较
Date.from_iso8601("2021-10-31") > Date.from_iso8601("2021-10-30")
# 输出: true
```

## 深入探讨:

历史背景: 日期解析功能在电子数据处理发展初期就已经存在，早期的计算机系统中只能处理数值数据，无法直接处理日期信息。因此，需要将日期转换为数值数据，然后再进行计算。随着计算机技术的发展，日期解析功能也不断优化，现在已经成为各种编程语言中的常见功能。

其他选择: 解析日期字符串还有其他方法，例如使用正则表达式或自定义函数。但是，Elixir 中的日期解析功能可以更快捷地解析各种日期格式，且支持不同的时区和语言。

实现细节: 在 Elixir 中，日期解析功能使用了 DateTime 模块来处理日期和时间的转换。它还使用了 ISO 8601 标准来确保日期字符串的正确解析。

## 另请参阅:

- [DateTime 模块官方文档](https://hexdocs.pm/elixir/DateTime.html)
- [ISO 8601 标准文档](https://www.iso.org/iso-8601-date-and-time-format.html)