---
title:                "Elixir: 将日期转换为字符串"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 为什么要将日期转换为字符串

在编程中，日期和时间的处理是一个常见的需求。无论是在记录数据、处理日程表还是其他场景，我们总是需要将日期按照特定的格式展示出来。Elixir提供了方便的方法来将日期转换为字符串，让我们一起来看看吧。

## 如何操作

要将日期转换为字符串，我们可以使用`~D`标识符来指定日期格式。假设我们有一个日期变量`date`，我们可以使用`~D`来将它转换为字符串，如下所示：

```Elixir
date = ~D[2021-03-10] 
```

在这个例子中，我们将日期格式指定为`YYYY-MM-DD`，也就是年、月、日的顺序。接下来，让我们使用`~w`标识符来将日期转换为字符串形式的星期几，代码如下：

```Elixir
date = ~D[2021-03-10]
~w(date)
```

这样就会得到`"Wednesday"`的输出。我们还可以将日期转换为其他常见的格式，例如`YYYY年MM月DD日`，`MM/DD/YY`等等。只需要根据自己的需求来指定格式即可。

## 深入了解

在Elixir中，日期和时间的处理采用了Erlang的标准库`:calendar`。因此，在进行日期转换的时候，我们可以使用`~D`来指定日期格式，也可以使用`:calendar`模块中的相关函数来进行更复杂的操作。例如，我们可以使用`Calendar.format`函数来将日期转换为指定格式的字符串，代码如下所示：

```Elixir
date = ~D[2021-03-10]
Calendar.format(date, "{YYYY年MM月DD日}", :abbreviated)
```

在这个例子中，我们使用`abbreviated`来指定中国的简写格式。除了`format`函数外，`:calendar`模块还提供了许多其他有用的函数，例如`is_datetime/1`用于检查是否为合法的日期时间格式，`today/0`用于获取当前日期等等。有兴趣的读者可以查阅相关文档来进一步学习。

# 此外可参考

- [Elixir官方文档：日期和时间](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir中国社区：日期和时间简介](https://elixir-cn.com/1.3/date-and-time.html)
- [Erlang官方资料：:calendar模块](http://erlang.org/doc/man/calendar.html)