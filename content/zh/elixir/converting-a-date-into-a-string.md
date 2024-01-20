---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

日期转字符串是将日期数据类型转换为可读形式的过程。程序员之所以这么做，是为了更便捷地显示和操作日期信息。

## 如何操作:

在Elixir中，我们通常使用DateTime模块进行转换。代码示例如下：

```Elixir
iex> dt = DateTime.utc_now()
iex> to_string(dt)
"2022-03-29 04:12:25.123Z"
```

上述代码首先生成一个当前的 UTC 时间的 DateTime 对象，然后通过`to_string`函数将其转换为字符串。

## Deep Dive

将日期转为字符串在历史上一直是一种常见需求。例如，早期用于打印到控制台或显示在用户界面中。在能够解析日期字符串的语言中（如Javascript），这也是数据序列化的方法。

当然我们也有其他方式来转换：你可以使用 DateFormat模块或者直接使用基础函数，如：`to_char`，来自定义你的格式化输出。

实际上，在Elixir中会存在多种日期类型，DateTime只是其中之一。你可以将NaiveDateTime，Time，Date等类型都转为字符串。只需要保证你的数据在转换前是有效的即可。

## 参见

1. Elixir DateTime官方文档: [https://hexdocs.pm/elixir/DateTime.html](https://hexdocs.pm/elixir/DateTime.html)
3. Elixir基础函数: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)