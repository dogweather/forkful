---
title:                "Elixir: 计算未来或过去的日期"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么会计算未来或过去的日期？

在编程中，我们经常需要计算未来或过去的日期，例如，计算一个任务的到期日期或生日在未来几年的日期。使用Elixir编程语言可以轻松地进行这样的计算，让我们来看看如何做到这一点。

## 如何做？

要计算未来或过去的日期，我们需要使用Elixir标准库中的Date和Timex模块。首先，我们需要导入这两个模块，然后使用Date.add/3函数来添加或减去指定的天数，月数或年数。例如，如果我们想计算10天后的日期，我们可以这样写：

```Elixir
import Date
import Timex

Date.add(Date.utc_now, 10, :day)
```

这将返回一个Date结构，其中包含10天后的日期。我们还可以指定月数和年数来计算未来或过去的日期。例如，如果我们想计算2个月前的日期，我们可以这样写：

```Elixir
Date.add(Date.utc_now, -2, :month)
```

此外，我们还可以使用Timex模块中的format/2来将日期格式化为我们想要的样式。例如，我们可以将前面计算出的日期格式化为'YYYY-MMM-DD'的格式：

```Elixir
Date.utc_now
|> Date.add(10, :day)
|> Timex.format("{YYYY}-{M}-{DD}")
```

这将返回类似于"2021-Sep-15"的格式化日期字符串。

## 深入探讨

在Elixir中，日期和时间被表示为不可变的结构。因此，通过函数操作来改变日期或时间并不会改变原始的日期结构。此外，Date和Timex模块提供了许多其他函数来操作日期，例如比较两个日期的大小、获取两个日期之间的差异等。我们可以阅读官方文档来了解更多关于这两个模块的函数和用法。

## 查看更多

- [Date模块官方文档](https://hexdocs.pm/elixir/Date.html)
- [Timex模块官方文档](https://hexdocs.pm/timex/Timex.html)
- [Elixir编程语言官方网站](https://elixir-lang.org)

## 参考链接

- [How to work with dates and times in Elixir](https://medium.com/@catprintlabs/how-to-work-with-dates-and-times-in-elixir-1957f48466d9)
- [Manipulating dates and times in Elixir with Timex](https://dev.to/ccschmitz/manipulating-dates-and-times-in-elixir-with-timex-3g5e)
- [Elixir date functions cheatsheet](https://www.poeticoding.com/elixir-date-functions-cheatsheet/)