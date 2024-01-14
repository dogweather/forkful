---
title:                "Elixir: 获取当前日期"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

在编写任何程序时，获取当前日期和时间可能是一个必要的步骤。这可以告诉用户何时发生了某些事件，也可以帮助我们对程序进行调试。

## 怎么做

使用Elixir编程语言可以非常简单地获取当前日期和时间。首先，我们需要导入Elixir的日期和时间模块，代码如下：

```Elixir
import DateTime
```

然后，我们可以使用```now()```函数来获取当前的日期和时间，如下所示：

```Elixir
DateTime.now()
```

这将返回一个包含当前日期和时间信息的数据结构。我们还可以使用```now("UTC")```来获取世界协调时间（UTC）的当前日期和时间。

如果我们只需要获取日期或时间的某个特定部分，可以使用```Date```和```Time```模块的其他函数。例如，要获取当前年份，我们可以使用```Date.year()```函数。要获取当前小时数，我们可以使用```Time.hour()```函数。

下面是一个完整的示例代码和输出：

```Elixir
# 导入日期和时间模块
import DateTime

# 获取当前日期和时间
DateTime.now()

# 获取当前年份
Date.year(DateTime.now())

# 获取当前小时数
Time.hour(DateTime.now())
```

输出：

```
{:ok, #DateTime<2020-08-26 14:20:25.000000Z>}
2020
14
```

## 深入探讨

在Elixir中，日期和时间被表示为数字的元组。日期由年、月、日三个数字组成，时间由时、分、秒、毫秒四个数字组成。使用这种数据结构可以方便地进行日期和时间的计算和比较。

此外，Elixir还有许多其他的日期和时间处理函数，如计算两个日期之间的差异、将日期和时间转换为不同的格式等等。

## 参考链接

- Elixir官方文档：https://hexdocs.pm/elixir/DateTime.html
- Elixir日期和时间教程：https://elixircasts.io/working-with-dates-and-times-in-elixir
- Elixir日期和时间相关文章：https://blog.jetbrains.com/datalore/2020/07/27/working-with-dates-and-times-in-elixir/