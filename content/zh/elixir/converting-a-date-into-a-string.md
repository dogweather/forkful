---
title:                "将日期转换为字符串"
html_title:           "Elixir: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么
在编程过程中，有时候我们需要将日期转换成字符串来方便数据的处理，这篇文章将向您介绍如何使用Elixir来完成这一任务。

## 如何操作
首先，我们需要使用`DateTime`模块来获取日期和时间的信息。如下所示，我们可以用`DateTime.now()`来获取当前的日期和时间，并将其赋值给一个变量`date`。

```elixir
date = DateTime.now()
```

接下来，我们可以使用`DateTime`模块中的函数`to_string`来将日期转换成字符串。例如，我们可以将`date`变量中的日期转换成一个完整的时间戳字符串。

```elixir
timestamp = DateTime.to_string(date, "~c")
# 2020 24 Dec, 12:00:00.000 AM
```

我们也可以使用特定的格式来转换日期，比如转换成`YYYY-MM-DD`的格式。

```elixir
formatted_date = DateTime.to_string(date, "YYYY-MM-DD")
# 2020-12-24
```

## 深入了解
Elixir中的日期和时间处理十分灵活，可以使用不同的格式和函数来满足不同的需求。除了`to_string`函数，`DateTime`模块还提供了其他方便的函数来进行日期的转换和处理。 例如，`DateTime.from_iso8601`函数可以用来解析ISO 8601格式的日期字符串。

```elixir
iso_string = "2020-12-24T12:00:00Z"
iso_date = DateTime.from_iso8601(iso_string)
# 2020-12-24 12:00:00Z
```

您也可以使用`DateTime.truncate`函数来将日期时间截断到特定的精度，例如到分钟或秒。

```elixir
DateTime.truncate(date, :minute)
# #DateTime<2020-12-24 00:00:00Z>
DateTime.truncate(date, :second)
# #DateTime<2020-12-24 00:00:00Z>
```

## 参考资料
- [`DateTime`模块文档](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir日期和时间处理指南](https://elixirschool.com/lessons/basics/date-and-time/)

## 查看也有
- [Elixir官方文档](https://elixir-lang.org/docs.html)
- [Elixir中国社区](https://elixir-cn.com/)