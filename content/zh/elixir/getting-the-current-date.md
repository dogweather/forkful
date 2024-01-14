---
title:                "Elixir: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

#为什么获取当前日期？
对于Elixir程序员来说，获取当前日期是一个非常常见的任务。它可以用于记录日志、确定运行时间或在需要时对时间进行计算。

## 如何实现？
在Elixir中，我们可以使用内置的Date和Time模块来获取当前日期和时间。下面是一个简单的示例代码：

```Elixir
date = Date.utc_today()
time = Time.utc_now()

IO.puts "今天是：#{date.yyyymmdd}"
IO.puts "现在是：#{time.hhmmss}"
```

运行结果将打印出当前日期和时间，例如：

>今天是：2020-09-28
>现在是：13:25:42

我们还可以使用Date和Time模块中的其他函数来进一步操作日期和时间。例如，我们可以使用Date.add函数来增加特定的时间段：

```Elixir
date = Date.utc_today()
future = Date.add(date, 1, "day")

IO.puts "明天是：#{future.yyyymmdd}"
```

这将打印出明天的日期，例如：

>明天是：2020-09-29

## 深入了解
Elixir的Date和Time模块是基于Erlang库而构建的。因此，它们遵循相同的API和数据类型。这些模块提供了广泛的函数来操作日期和时间，使得它们非常灵活和强大。

另外，Elixir还有许多其他的日期和时间相关的库，例如Calendar和Timex。每个库都有其独特的功能和优点，可以根据具体需求进行选择。

# 参考链接
- [Elixir官方文档](https://hexdocs.pm/elixir/Date.html)
- [Erlang官方文档](https://erlang.org/doc/man/calendar.html)
- [Timex库](https://hexdocs.pm/timex/1.5.4/Timex.html)
- [Calendar库](https://github.com/lau/calendar)