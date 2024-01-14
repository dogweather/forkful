---
title:    "Elixir: 计算未来或过去的日期"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# 为什么

在编程世界中，有时候我们需要计算日期的变化，比如计算将来的某个日期或者过去的某个日期。这可能是为了计算过去的某个事件发生多久了，或者未来某个项目的截止日期是什么时候。在Elixir编程语言中，我们可以轻松地实现这样的日期计算功能，让我们来看看如何做到这一点吧！

# 怎么做

在Elixir中，我们可以使用`Calendar`模块来处理日期。首先，让我们导入`Calendar`模块，然后使用`to_gregorian_days`函数将日期转换为格里高利历的天数，从而方便我们进行日期的计算。例如，我们可以计算未来30天后的日期，代码如下所示：

```Elixir
iex> require Calendar
Calendar
iex> date = {2021, 7, 1}
{2021, 7, 1}
iex> future_date = Calendar.to_gregorian_days(date) + 30
2459446
iex> Calendar.from_gregorian_days(future_date)
{2021, 7, 31}
```

这里，我们首先定义了一个日期变量`date`，然后使用`to_gregorian_days`函数将其转换为格里高利历的天数。接着，我们将这个天数加上30，得到未来的天数`future_date`。最后，我们再次使用`Calendar`模块的函数`from_gregorian_days`来将未来的天数转换为日期格式。如此简单，我们就可以计算未来的日期了！

# 深入探讨

除了简单的日期计算，Elixir中的`Calendar`模块还提供了其他功能，比如计算两个日期之间的差值。我们可以使用`date.diff(future_date)`来计算两个日期之间相差的天数。另外，我们也可以使用`date.add(days)`和`date.sub(days)`来在指定的日期上增加或减少一定的天数。这些功能可以为我们处理复杂的日期计算提供便利。

除了`Calendar`模块，Elixir的第三方库`Timex`也提供了更多的日期处理功能，如计算工作日、节假日和时区转换等。如果你需要更多高级的日期功能，可以尝试使用`Timex`库。

# 参考链接

- Elixir官方文档：https://elixir-lang.org/getting-started/calendar-and-timestamps.html
- Timex库文档：https://hexdocs.pm/timex/readme.html

## 参见

- [Elixir和Erlang的关系（中文）](https://github.com/sunyshhh/articles/blob/master/Elixir%E5%92%8CErlang%E7%9A%84%E5%85%B3%E7%B3%BB.md)
- [如何学习Elixir（中文）](https://github.com/sunyshhh/articles/blob/master/%E5%A6%82%E4%BD%95%E5%AD%A6%E4%B9%A0Elixir.md)
- [Elixir的常见面试题（中文）](https://github.com/sunyshhh/articles/blob/master/Elixir%E7%9A%84%E5%B8%B8%E8%A7%81%E9%9D%A2%E8%AF%95%E9%A2%98.md)