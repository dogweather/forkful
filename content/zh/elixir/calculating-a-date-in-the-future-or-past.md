---
title:    "Elixir: 计算将来或过去的日期"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么要计算未来或过去的日期？

计算未来或过去的日期在编程中是很常见的需求，比如说我们需要根据用户的生日来计算他的年龄，或者根据某个事件的发生日期来计算距离今天已经过了多少天。这样的计算可以帮助我们更好地处理和理解时间的概念。

## 如何进行日期计算

在Elixir中，我们可以使用`DateTime`模块来进行日期的计算。首先，我们需要导入这个模块，然后使用`DateTime.from_naive`函数来将我们提供的日期转换为Elixir的日期类型。

```elixir
import DateTime

DateTime.from_naive({2000, 12, 31}, "Etc/UTC")
#=> {:ok, ~U[2000-12-31 00:00:00Z]}
```

接下来，我们可以使用`DateTime.add`函数来对日期进行加减操作，第一个参数为原始日期，第二个参数为我们要加减的时间量。例如，我们可以在当前日期加上3天：

```elixir
DateTime.add(~U[2022-01-01 00:00:00Z], {3, :days})
#=> ~U[2022-01-04 00:00:00Z]
```

我们也可以对不同的时间单位进行加减操作，比如说：

```elixir
DateTime.add(~U[2022-01-01 00:00:00Z], {1, :weeks})
#=> ~U[2022-01-08 00:00:00Z]

DateTime.add(~U[2022-01-01 00:00:00Z], {2, :months})
#=> ~U[2022-03-01 00:00:00Z]
```

## 深入了解日期计算

Elixir的`DateTime`模块还提供了很多其他有用的函数，比如说`DateTime.diff`用于计算两个日期之间的时间差，`DateTime.compare`用于比较两个日期的大小，`DateTime.truncate`用于去除日期或时间中的某些部分，等等。

如果我们需要在代码中频繁使用日期计算，我们也可以考虑使用`Timex`这个第三方库。它提供了更多的日期计算函数和格式化选项，可以帮助我们更方便地处理日期数据。

## 查看更多

- [Elixir官方文档 - DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir官方文档 - Timex](https://hexdocs.pm/timex/readme.html)
- [Elixir时间和日期处理的最佳实践](https://medium.com/@eigenjoy/elixir-time-and-date-handling-best-practices-e7192479956b)