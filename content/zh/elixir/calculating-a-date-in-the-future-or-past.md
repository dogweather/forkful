---
title:                "计算未来或过去的日期"
html_title:           "Elixir: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 什么和为什么？
计算未来或过去的日期是指在程序中使用语言来计算某一日期之后或之前的日期。程序员们通常会在日程安排或日历应用中使用这种方法。

# 如何：
```Elixir
# 计算未来的日期
Future.calc(date, days)
# 示例输入：Future.calc("2021-09-01", 5)
# 预期输出：2021-09-06

# 计算过去的日期
Past.calc(date, days)
# 示例输入：Past.calc("2021-09-06", 5)
# 预期输出：2021-09-01
```

# 深入挖掘
计算未来或过去的日期在历史上一直是程序员们经常用到的技巧。除了使用Elixir语言外，也可以通过其他语言来实现这一功能。常用的算法包括Julian计算法和格里高利历算法。在Elixir中，可以使用DateTime模块来进行日期计算。

# 参考资料
- [Elixir DateTime文档](https://hexdocs.pm/elixir/DateTime.html)
- [Julian计算法介绍](https://en.wikipedia.org/wiki/Julian_day)
- [格里高利历算法介绍](https://en.wikipedia.org/wiki/Gregorian_calendar)