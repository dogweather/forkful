---
title:                "Elixir: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在Elixir编程中，将日期转换为字符串是非常常见的操作。这可以让我们在处理日期数据时更加灵活，可以将其转换为不同的格式。例如，我们可以将日期转换为“年-月-日”的形式以便于显示或存储。

## 如何进行

要将日期转换为字符串，我们可以使用`to_string`函数并传入日期对象作为参数。以下是一个简单的示例，将今天的日期转换为“年-月-日”的格式。

```Elixir
date = Date.utc_today()
string_date = to_string(date, "{YYYY}-{MM}-{DD}")
IO.puts(string_date)
```

运行上面的代码将输出类似于`2021-07-08`的字符串。我们可以根据需要调整`to_string`函数的参数，比如要显示更多的时间信息，可以传入`{YYYY}-{MM}-{DD} {hh}:{mm}:{ss}`来包含小时、分钟和秒。

## 深度探究

在Elixir中，日期对象实际上是使用以下形式的元组存储的：`{year, month, day}`。当我们调用`to_string`函数时，它会将日期对象转换为一个字符串，根据我们传入的参数来决定输出的格式。事实上，我们也可以手动调用`DateTime.to_iso8601`函数来执行日期到字符串的转换。这个函数会将日期对象转换为ISO8601标准的字符串，包含年、月、日、小时、分钟、秒、时区等信息。

除了`to_string`和`DateTime.to_iso8601`，Elixir还提供了许多其他用于日期转换的函数，如`Date.to_erl`和`NaiveDateTime.to_unix`。如果你想深入了解日期转换的内部实现，可以查看Elixir的`Calendar`模块源代码。

## 参考链接

- [Elixir中的日期和时间](https://hexdocs.pm/elixir/Calendar.html)
- [Elixir Cookbook中的日期和时间处理](https://elixirschool.com/zh-hant/cn/lessons/advanced/date-time/)
- [Elixir学习资料汇总](https://github.com/h4cc/awesome-elixir)